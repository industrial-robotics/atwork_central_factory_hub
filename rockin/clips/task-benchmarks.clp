;---------------------------------------------------------------------------
;  task-benchmarks.clp - RoCKIn RefBox CLIPS task benchmarks
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass Tbm1InitState (is-a InitState))

(defmessage-handler Tbm1InitState on-update ()
  ; Inventory
  (slot-insert$ [inventory] items 1
    ;;;;;;;;;;;;;;;;;;;;
    ; Assembly aid trays
    ;;;;;;;;;;;;;;;;;;;;

    ; Assembly aid tray EM-01-02 at shelf SH-08
    (make-instance of Item (object-id [em-01-02]) (location-id [shelf-08]))

    ;;;;;;;;;;;;;;;
    ; Bearing boxes
    ;;;;;;;;;;;;;;;

    ; 1 bearing box at shelf SH-02
    (make-instance of Item (object-id [ax-01]) (location-id [shelf-02]) (quantity 1))

    ; 1 bearing box at shelf SH-20
    (make-instance of Item (object-id [ax-01]) (location-id [shelf-20]) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver 2 items of AX-01 to EM-01-02
    (make-instance of Order (status OFFERED) (object-id [ax-01]) (container-id [em-01-02]) (quantity-requested 2))

    ; Deliver container EM-01-02 to location WS-01
    (make-instance of Order (status OFFERED) (object-id [em-01-02]) (destination-id [workstation-01]))
  )

  (send [sm] process-event VALID_BENCHMARK)
)

(deffunction task-benchmarks-tbm1-init ()
  (make-instance [init-state] of Tbm1InitState)
  (make-instance [stopped-state] of StoppedState)
  (make-instance [running-state] of RunningState)
  (make-instance [paused-state] of PausedState)
  (make-instance [finished-state] of FinishedState)

  (send [init-state]       add-transition VALID_BENCHMARK [stopped-state])
  (send [stopped-state]    add-transition RESET           [init-state])
  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition RESET           [init-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [finished-state])
  (send [running-state]    add-transition FINISH          [finished-state])
  (send [paused-state]     add-transition RESET           [init-state])
  (send [paused-state]     add-transition CONTINUE        [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])
  (send [finished-state]   add-transition RESET           [init-state])

  (make-instance [sm] of StateMachine
    (current-state [init-state])
    (states [init-state] [stopped-state] [running-state] [paused-state] [finished-state])
  )
)




(defclass Tbm2InitState (is-a InitState))

(defmessage-handler Tbm2InitState on-update ()
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

    ;;;;;;;;;;;;;;;;;;;;;;
    ; Manipulation objects
    ;;;;;;;;;;;;;;;;;;;;;;

    ; File-card box EM-02-02 in shelf SH-14
    (make-instance of Item (object-id [em-02-02]) (location-id [shelf-14]) (quantity 1))

    ; File-card box EM-02-02 on robot
    (make-instance of Item (object-id [em-02-02]) (location-id [robot]) (quantity 1))

    ; Common shelf container ER-02-04 in location CB-01
    (make-instance of Item (object-id [er-02-04]) (location-id [conveyor_belt-01]) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver file-card box EM-02-02 to workstation WS-05
    (make-instance of Order (status OFFERED) (object-id [em-02-02]) (destination-id [workstation-05]))

    ; Deliver 5 machined cover plates AX-07 into file-card box EM-02-02
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-02]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-02]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-02]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-02]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-02]) (quantity-requested 1))

    ; Deliver 5 unusable cover plates AX-08 into common shelf container ER-02-04
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-04]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-04]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-04]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-04]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-04]) (quantity-requested 1))
  )

  (send [sm] process-event VALID_BENCHMARK)
)

(deffunction task-benchmarks-tbm2-init ()
  (make-instance [init-state] of Tbm2InitState)
  (make-instance [stopped-state] of StoppedState)
  (make-instance [running-state] of RunningState)
  (make-instance [paused-state] of PausedState)
  (make-instance [finished-state] of FinishedState)

  (send [init-state]       add-transition VALID_BENCHMARK [stopped-state])
  (send [stopped-state]    add-transition RESET           [init-state])
  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition RESET           [init-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [finished-state])
  (send [running-state]    add-transition FINISH          [finished-state])
  (send [paused-state]     add-transition RESET           [init-state])
  (send [paused-state]     add-transition CONTINUE        [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])
  (send [finished-state]   add-transition RESET           [init-state])

  (make-instance [sm] of StateMachine
    (current-state [init-state])
    (states [init-state] [stopped-state] [running-state] [paused-state] [finished-state])
  )
)




(defclass Tbm3InitState (is-a InitState))

(defmessage-handler Tbm3InitState on-update ()
  ; Inventory
  (slot-insert$ [inventory] items 1
    ;;;;;;;;;;
    ; Objects
    ;;;;;;;;;;

    ; 1 bearing box AX-01 at shelf SH-02
    (make-instance of Item (object-id [ax-01]) (location-id [shelf-02]) (quantity 1))

    ; 1 bearing AX-02 at shelf SH-07
    (make-instance of Item (object-id [ax-02]) (location-id [shelf-07]) (quantity 1))

    ; 1 axis AX-03 at shelf SH-09
    (make-instance of Item (object-id [ax-03]) (location-id [shelf-09]) (quantity 1))

    ; 1 shaft nut AX-04 at shelf SH-13
    (make-instance of Item (object-id [ax-04]) (location-id [shelf-13]) (quantity 1))

    ; 1 distance tube AX-05 at shelf SH-15
    (make-instance of Item (object-id [ax-05]) (location-id [shelf-15]) (quantity 1))

    ; 1 motor with gear box AX-09 at shelf SH-21
    (make-instance of Item (object-id [ax-09]) (location-id [shelf-21]) (quantity 1))

    ;;;;;;;;;;;;;;;;
    ; Foam container
    ;;;;;;;;;;;;;;;;

    ; Foam container EM-03-01 at shelf SH-01
    (make-instance of Item (object-id [em-03-01]) (location-id [shelf-01]) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver foam container EM-03-01 to workstation WS-04
    (make-instance of Order (status OFFERED) (object-id [em-03-01]) (destination-id [workstation-04]))

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

  (send [sm] process-event VALID_BENCHMARK)
)

(deffunction task-benchmarks-tbm3-init ()
  (make-instance [init-state] of Tbm3InitState)
  (make-instance [stopped-state] of StoppedState)
  (make-instance [running-state] of RunningState)
  (make-instance [paused-state] of PausedState)
  (make-instance [finished-state] of FinishedState)

  (send [init-state]       add-transition VALID_BENCHMARK [stopped-state])
  (send [stopped-state]    add-transition RESET           [init-state])
  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition RESET           [init-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [finished-state])
  (send [running-state]    add-transition FINISH          [finished-state])
  (send [paused-state]     add-transition RESET           [init-state])
  (send [paused-state]     add-transition CONTINUE        [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])
  (send [finished-state]   add-transition RESET           [init-state])

  (make-instance [sm] of StateMachine
    (current-state [init-state])
    (states [init-state] [stopped-state] [running-state] [paused-state] [finished-state])
  )
)

