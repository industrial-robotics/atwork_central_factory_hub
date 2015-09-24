;---------------------------------------------------------------------------
;  task-benchmarks.clp - RoCKIn RefBox CLIPS task benchmarks
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deffunction task-benchmarks-tbm1-init (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*TBM-TIME*))
  (make-instance [paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [finished-state])
  (send [running-state]    add-transition FINISH          [finished-state])
  (send [paused-state]     add-transition START           [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])

  (make-instance ?state-machine of StateMachine
    (current-state [stopped-state])
    (states [stopped-state] [running-state] [paused-state] [finished-state])
  )


  ; Inventory
  (slot-insert$ [inventory] items 1
    ;;;;;;;;;;;;;;;;;;;;
    ; Assembly aid trays
    ;;;;;;;;;;;;;;;;;;;;

    ; Assembly aid tray EM-01-01 at shelf SH-07
    (make-instance of Item (object-id [em-01-01]) (location-id [shelf-07]))

    ;;;;;;;;;;;;;;;
    ; Bearing boxes
    ;;;;;;;;;;;;;;;

    ; 1 bearing box at shelf SH-02
    (make-instance of Item (object-id [ax-01]) (location-id [shelf-02]) (quantity 1))

    ; 1 bearing box at shelf SH-09
    (make-instance of Item (object-id [ax-01]) (location-id [shelf-09]) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver 2 items of AX-01 to EM-01-01
    (make-instance of Order (status OFFERED) (object-id [ax-01]) (container-id [em-01-01]) (quantity-requested 2))

    ; Deliver container EM-01-01 to location WS-01
    (make-instance of Order (status OFFERED) (object-id [em-01-01]) (destination-id [workstation-01]))
  )
)




(deffunction task-benchmarks-tbm2-init (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*TBM-TIME*))
  (make-instance [paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [finished-state])
  (send [running-state]    add-transition FINISH          [finished-state])
  (send [paused-state]     add-transition START           [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])

  (make-instance ?state-machine of StateMachine
    (current-state [stopped-state])
    (states [stopped-state] [running-state] [paused-state] [finished-state])
  )


  ; Inventory
  (slot-insert$ [inventory] items 1
    ;;;;;;;;;;;;;;
    ; Cover plates
    ;;;;;;;;;;;;;;

    ; 5 cover plates with unknown state on conveyor belt CB-01
    (make-instance of Item (object-id [ax-15]) (location-id [conveyor_belt-01]) (quantity 1))
    (make-instance of Item (object-id [ax-15]) (location-id [conveyor_belt-01]) (quantity 1))
    (make-instance of Item (object-id [ax-15]) (location-id [conveyor_belt-01]) (quantity 1))
    (make-instance of Item (object-id [ax-15]) (location-id [conveyor_belt-01]) (quantity 1))
    (make-instance of Item (object-id [ax-15]) (location-id [conveyor_belt-01]) (quantity 1))

    ;;;;;;;;;;;;;;;;;;;;;;
    ; Manipulation objects
    ;;;;;;;;;;;;;;;;;;;;;;

    ; File-card box EM-02-01 in shelf SH-02
    (make-instance of Item (object-id [em-02-01]) (location-id [shelf-02]) (quantity 1))

    ; File-card box EM-02-01 on robot
    ;(make-instance of Item (object-id [em-02-01]) (location-id [robot]) (quantity 1))

    ; Common shelf container ER-02-01 in location CB-01
    (make-instance of Item (object-id [er-02-01]) (location-id [conveyor_belt-01]) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver file-card box EM-02-01 to workstation WS-05
    (make-instance of Order (status OFFERED) (object-id [em-02-01]) (destination-id [workstation-05]))

    ; Deliver 5 machined cover plates AX-07 into file-card box EM-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-01]) (quantity-requested 1))

    ; Deliver 5 unusable cover plates AX-08 into common shelf container ER-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-01]) (quantity-requested 1))
  )
)




(deffunction task-benchmarks-tbm3-init (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*TBM-TIME*))
  (make-instance [paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [finished-state])
  (send [running-state]    add-transition FINISH          [finished-state])
  (send [paused-state]     add-transition START           [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])

  (make-instance ?state-machine of StateMachine
    (current-state [stopped-state])
    (states [stopped-state] [running-state] [paused-state] [finished-state])
  )


  ; Inventory
  (slot-insert$ [inventory] items 1
    ;;;;;;;;;;
    ; Objects
    ;;;;;;;;;;

    ; 1 bearing box AX-01 at shelf SH-01
    (make-instance of Item (object-id [ax-01]) (location-id [shelf-01]) (quantity 1))

    ; 1 bearing AX-02 at shelf SH-02
    (make-instance of Item (object-id [ax-02]) (location-id [shelf-02]) (quantity 1))

    ; 1 axis AX-03 at shelf SH-03
    (make-instance of Item (object-id [ax-03]) (location-id [shelf-03]) (quantity 1))

    ; 1 shaft nut AX-04 at shelf SH-07
    (make-instance of Item (object-id [ax-04]) (location-id [shelf-07]) (quantity 1))

    ; 1 distance tube AX-05 at shelf SH-08
    (make-instance of Item (object-id [ax-08]) (location-id [shelf-08]) (quantity 1))

    ; 1 motor with gear box AX-09 at shelf SH-09
    (make-instance of Item (object-id [ax-09]) (location-id [shelf-09]) (quantity 1))

    ;;;;;;;;;;;;;;;;
    ; Foam container
    ;;;;;;;;;;;;;;;;

    ; Foam container EM-03-01 at shelf WS-01
    (make-instance of Item (object-id [em-03-01]) (location-id [workstation-01]) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver foam container EM-03-01 to workstation WS-03
    (make-instance of Order (status OFFERED) (object-id [em-03-01]) (destination-id [workstation-03]))

    ; Deliver 1 bearing box AX-01 into foam container EM-03-01
    (make-instance of Order (status OFFERED) (object-id [ax-01]) (container-id [em-03-01]) (quantity-requested 1))

    ; Deliver 1 bearing AX-02 into foam container EM-03-01
    (make-instance of Order (status OFFERED) (object-id [ax-02]) (container-id [em-03-01]) (quantity-requested 1))

    ; Deliver 1 axis AX-03 into foam container EM-03-01
    (make-instance of Order (status OFFERED) (object-id [ax-03]) (container-id [em-03-01]) (quantity-requested 1))

    ; Deliver 1 shaft nut AX-04 into foam container EM-03-01
    (make-instance of Order (status OFFERED) (object-id [ax-04]) (container-id [em-03-01]) (quantity-requested 1))

    ; Deliver 1 distance tube AX-05 into foam container EM-03-01
    (make-instance of Order (status OFFERED) (object-id [ax-05]) (container-id [em-03-01]) (quantity-requested 1))

    ; Deliver 1 motor with gear box AX-09 into foam container EM-03-01
    (make-instance of Order (status OFFERED) (object-id [ax-09]) (container-id [em-03-01]) (quantity-requested 1))
  )
)
