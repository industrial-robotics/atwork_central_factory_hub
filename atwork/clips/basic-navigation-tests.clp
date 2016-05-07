;---------------------------------------------------------------------------
;  task-benchmarks.clp - RoCKIn RefBox CLIPS task benchmarks
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicNavigationTest1 (is-a BenchmarkScenario) (role concrete))

(defmessage-handler BasicNavigationTest1 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BNT-EXECUTION-TIME*))
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


  (bind ?shelf-locations (create$ [shelf-01] [shelf-02] [shelf-03] [shelf-04]
        [shelf-05] [shelf-06] [shelf-07] [shelf-08] [shelf-09] [shelf-10]
        [shelf-11] [shelf-12]))

  ; Randomize a location for ER-02-01
  (bind ?er-02-01-location (pick-random$ ?shelf-locations))

  ; The location of the assembly aid tray should should not be reused
  (bind ?shelf-locations (delete-member$ ?shelf-locations ?er-02-01-location))

  ; Inventory
  ;(slot-insert$ [inventory] items 1
    ;;;;;;;;;;;;;;;;
    ; Foam container
    ;;;;;;;;;;;;;;;;

    ; Foam container ER-02-01 at random location
    ;(make-instance of Item (object-id [F20_20_B]) (location-id ?er-02-01-location) (quantity 1))

    ;;;;;;;;;;
    ; Objects
    ;;;;;;;;;;

    ; 1 motor with gear box AX-09 at random location
    ;(make-instance of Item (object-id [F20_20_B]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))
  ;)


  ; Orders
  (slot-insert$ [order-info] orders 1
	    ; Deliver foam container ER-02-01 to workstation WS-06
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL]) (destination-id [workstation-06])
                            (wait-time 5) (orientation WEST))

    ; Deliver 1 bearing box AX-01 into foam container ER-02-01
    ;(make-instance of Order (status OFFERED) (object-id [ax-01]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 bearing AX-02 into foam container ER-02-01
    ;(make-instance of Order (status OFFERED) (object-id [ax-02]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 axis AX-03 into foam container ER-02-01
    ;(make-instance of Order (status OFFERED) (object-id [ax-03]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 shaft nut AX-04 into foam container ER-02-01
    ;(make-instance of Order (status OFFERED) (object-id [ax-04]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 distance tube AX-05 into foam container ER-02-01
    ;(make-instance of Order (status OFFERED) (object-id [ax-05]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 motor with gear box AX-09 into foam container ER-02-01
    ;(make-instance of Order (status OFFERED) (object-id [ax-09]) (container-id [er-02-01]) (quantity-requested 1))
  )
)

(defmessage-handler BasicNavigationTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)




(defrule init-tbm
  (init)
  ?bm <- (object (is-a Benchmark))
  =>
  (make-instance [BNT1] of BasicNavigationTest1 (type BNT) (type-id 1) (description "Basic Navigation Test 1"))

  (slot-insert$ ?bm registered-scenarios 1 [BNT1])
)
