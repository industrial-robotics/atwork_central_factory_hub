;---------------------------------------------------------------------------
;  device-force-fitting-machine.clp - RoCKIn RefBox CLIPS - Force fitting
;                                     machine interface
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defrule net-recv-ForceFittingMachineCommand
  ?mf <- (protobuf-msg (type "rockin_msgs.ForceFittingMachineCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (robot (name ?name) (team ?team) (host ?host))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case MOVE_UP then
      (printout t "Robot " ?name "/" ?team " commands force-fitting machine up" crlf)
      (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands force-fitting machine up"))))
      (force-fitting-machine-move-drill-up)
    )

    (case MOVE_DOWN then
      (printout t "Robot " ?name "/" ?team " commands force-fitting machine down" crlf)
      (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands force-fitting machine down"))))
      (force-fitting-machine-move-drill-down)
    )
  )
)

(defrule net-recv-ForceFittingMachineCommand-client
  ?mf <- (protobuf-msg (type "rockin_msgs.ForceFittingMachineCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (network-client (id ?client-id) (host ?client-host) (port ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case MOVE_UP then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands force-fitting machine up" crlf)
      (force-fitting-machine-move-drill-up)
    )

    (case MOVE_DOWN then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands force-fitting machine down" crlf)
      (force-fitting-machine-move-drill-down)
    )
  )
)

(deffunction net-create-ForceFittingMachineStatus ()
  ; Instantiate a new status message
  (bind ?pb-status (pb-create "rockin_msgs.ForceFittingMachineStatus"))

  ; Assumption: The enum in the device communication message is aligned with the
  ;             enum of the robot communication (e.g. in both messages the state
  ;             UNKNOWN maps to the value 4)
  (switch (force-fitting-machine-get-state)
    (case 0 then (pb-set-field ?pb-status "state" AT_TOP))
    (case 1 then (pb-set-field ?pb-status "state" AT_BOTTOM))
    (case 2 then (pb-set-field ?pb-status "state" MOVING_UP))
    (case 3 then (pb-set-field ?pb-status "state" MOVING_DOWN))
    (case 4 then (pb-set-field ?pb-status "state" UNKNOWN))
  )

  (return ?pb-status)
)

(defrule net-send-ForceFittingMachineStatus
  (time $?now)
  ?f <- (signal (type force-fitting-machine) (time $?t&:(timeout ?now ?t ?*FORCE-FITTING-MACHINE-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))

  (bind ?ds (net-create-ForceFittingMachineStatus))

  ; Broadcast to peers
  (pb-broadcast ?peer-id-public ?ds)

  ; Send to all clients
  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?ds)
  )

  (pb-destroy ?ds)
)
