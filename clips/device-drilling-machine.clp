;---------------------------------------------------------------------------
;  device-drilling-machine.clp - RoCKIn RefBox CLIPS - Drilling machine interface
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------


(defrule net-recv-DrillingMachineCommand
  ?mf <- (protobuf-msg (type "rockin_msgs.DrillingMachineCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (robot (name ?name) (team ?team) (host ?host))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case MOVE_UP then
      (printout t "Robot " ?name "/" ?team " commands drilling machine up" crlf)
      (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands drilling machine up"))))
      (drilling-machine-move-drill-up)
    )

    (case MOVE_DOWN then
      (printout t "Robot " ?name "/" ?team " commands drilling machine down" crlf)
      (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands drilling machine down"))))
      (drilling-machine-move-drill-down)
    )
  )
)

(defrule net-recv-DrillingMachineCommand-client
  ?mf <- (protobuf-msg (type "rockin_msgs.DrillingMachineCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (network-client (id ?client-id) (host ?client-host) (port ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case MOVE_UP then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands drilling machine up" crlf)
      (drilling-machine-move-drill-up)
    )

    (case MOVE_DOWN then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands drilling machine down" crlf)
      (drilling-machine-move-drill-down)
    )
  )
)

(deffunction net-create-DrillingMachineStatus ()
  ; Instantiate a new status message
  (bind ?pb-status (pb-create "rockin_msgs.DrillingMachineStatus"))

  ; Assumption: The enum in the device communication message is aligned with the
  ;             enum of the robot communication (e.g. in both messages the state
  ;             UNKNOWN maps to the value 4)
  (switch (drilling-machine-get-state)
    (case 0 then (pb-set-field ?pb-status "state" AT_TOP))
    (case 1 then (pb-set-field ?pb-status "state" AT_BOTTOM))
    (case 2 then (pb-set-field ?pb-status "state" MOVING_UP))
    (case 3 then (pb-set-field ?pb-status "state" MOVING_DOWN))
    (case 4 then (pb-set-field ?pb-status "state" UNKNOWN))
  )

  (return ?pb-status)
)

(defrule net-send-DrillingMachineStatus
  (time $?now)
  ?f <- (signal (type drilling-machine) (time $?t&:(timeout ?now ?t ?*DRILLING-MACHINE-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))

  (bind ?ds (net-create-DrillingMachineStatus))

  ; Broadcast to peers
  (pb-broadcast ?peer-id-public ?ds)

  ; Send to all clients
  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?ds)
  )

  (pb-destroy ?ds)
)
