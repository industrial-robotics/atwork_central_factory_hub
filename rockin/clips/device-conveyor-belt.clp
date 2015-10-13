;---------------------------------------------------------------------------
;  device-conveyor-belt.clp - RoCKIn RefBox CLIPS - Conveyor belt interface
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defrule net-recv-ConveyorBeltCommand-client
  ?mf <- (protobuf-msg (type "rockin_msgs.ConveyorBeltCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (network-client (id ?client-id) (host ?client-host) (port ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case STOP then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands conveyor belt to stop" crlf)
      (conveyor-belt-stop-belt)
    )

    (case START then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands conveyor belt to start" crlf)
      (conveyor-belt-start-belt)
    )
  )
)

(deffunction net-create-ConveyorBeltStatus ()
  ; Instantiate a new status message
  (bind ?pb-status (pb-create "rockin_msgs.ConveyorBeltStatus"))

  (if (conveyor-belt-is-running)
   then
    (pb-set-field ?pb-status "state" START)
   else
    (pb-set-field ?pb-status "state" STOP)
  )

  (return ?pb-status)
)

(defrule net-send-ConveyorBeltStatus
  (time $?now)
  ?f <- (signal (type conveyor-belt) (time $?t&:(timeout ?now ?t ?*CONVEYOR-BELT-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))

  (bind ?status (net-create-ConveyorBeltStatus))

  ; Send to all clients
  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?status)
  )

  (pb-destroy ?status)
)



(defrule net-recv-TriggeredConveyorBeltCommand
  ?mf <- (protobuf-msg (type "rockin_msgs.TriggeredConveyorBeltCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (robot (name ?name) (team ?team) (host ?host))
  ?cb <- (triggered-conveyor-belt (cycle ?current-cycle))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))
  (bind ?pb-next-cycle (pb-field-value ?p "next_cycle"))

  ; Peers may only start the conveyor belt
  (if (eq ?pb-command STOP)
   then
    (if (debug 3) then (printout t "Ignoring invalid conveyor belt command " ?pb-command " from robot " ?name "/" ?team crlf))
    (return)
  )

  ; Peers must provide the right cycle to command the conveyor belt
  (if (neq ?pb-next-cycle (+ ?current-cycle 1))
   then
    (printout t "Ignoring TriggeredConveyorBeltCommand due to invalid cycle" crlf)
    (if (debug 3) then (printout t "Ignoring conveyor belt command from robot " ?name "/" ?team " due to invalid cycle " crlf))
    (return)
  )

  ; The conveyor belt must not be running currently
  (if (conveyor-belt-is-running) then (return))

  (printout t "Robot " ?name "/" ?team " commands conveyor belt to start" crlf)
  (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands conveyor belt to start"))))
  (conveyor-belt-start-belt)

  (modify ?cb (cycle (+ ?current-cycle 1)))
)

(deffunction net-create-TriggeredConveyorBeltStatus (?current-cycle)
  ; Instantiate a new status message
  (bind ?pb-status (pb-create "rockin_msgs.TriggeredConveyorBeltStatus"))

  (if (conveyor-belt-is-running)
   then
    (pb-set-field ?pb-status "state" START)
   else
    (pb-set-field ?pb-status "state" STOP)
  )

  (pb-set-field ?pb-status "cycle" ?current-cycle)

  (return ?pb-status)
)

(defrule net-send-TriggeredConveyorBeltStatus
  (time $?now)
  ?f <- (signal (type triggered-conveyor-belt) (time $?t&:(timeout ?now ?t ?*CONVEYOR-BELT-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  (triggered-conveyor-belt (cycle ?current-cycle))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))

  (bind ?status (net-create-TriggeredConveyorBeltStatus ?current-cycle))

  ; Broadcast to peers
  (pb-broadcast ?peer-id-public ?status)

  (pb-destroy ?status)
)
