;---------------------------------------------------------------------------
;  task-benchmarks.clp - RoCKIn RefBox CLIPS task benchmarks
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass TaskBenchmark1 (is-a BenchmarkScenario) (role concrete))
(defclass TaskBenchmark2 (is-a BenchmarkScenario) (role concrete))
(defclass TaskBenchmark3 (is-a BenchmarkScenario) (role concrete))

(defmessage-handler TaskBenchmark1 setup (?time ?state-machine)
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


  (bind ?shelf-locations (create$ [shelf-01] [shelf-02] [shelf-03] [shelf-04]
        [shelf-05] [shelf-06] [shelf-07] [shelf-08] [shelf-09] [shelf-10]
        [shelf-11] [shelf-12]))

  ; Randomize a location for EM-01-01
  (bind ?em-01-01-location (pick-random$ ?shelf-locations))

  ; The location of EM-01-01 should should not be reused
  (bind ?shelf-locations (delete-member$ ?shelf-locations ?em-01-01-location))

  ; Inventory
  (slot-insert$ [inventory] items 1
    ;;;;;;;;;;;;;;;;;;;;
    ; Assembly aid trays
    ;;;;;;;;;;;;;;;;;;;;

    ; Assembly aid tray EM-01-01 at random location
    (make-instance of Item (object-id [em-01-01]) (location-id ?em-01-01-location))

    ;;;;;;;;;;;;;;;
    ; Bearing boxes
    ;;;;;;;;;;;;;;;

    ; 1 bearing box at random location
    (make-instance of Item (object-id [ax-01]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))

    ; 1 bearing box at random location
    (make-instance of Item (object-id [ax-01]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver 2 items of AX-01 to EM-01-01
    (make-instance of Order (status OFFERED) (object-id [ax-01]) (container-id [em-01-01]) (quantity-requested 2))

    ; Deliver container EM-01-01 to location WS-03
    (make-instance of Order (status OFFERED) (object-id [em-01-01]) (destination-id [workstation-03]))
  )
)

(defmessage-handler TaskBenchmark1 handle-feedback (?pb-msg ?time ?name ?team)
  (if (and
       (pb-has-field ?pb-msg "assembly_aid_tray_id")
       (pb-has-field ?pb-msg "container_id"))
   then
    (return CONTINUE)   ; TBM feedback is valid -> continue the benchmark
   else
    (return FINISH)     ; TBM feedback is invalid -> finish the benchmark
  )
)




(defmessage-handler TaskBenchmark2 setup (?time ?state-machine)
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
    ;;;;;;;;;;;;;;;;;;;;;;
    ; Manipulation objects
    ;;;;;;;;;;;;;;;;;;;;;;

    ; File-card box EM-02-01 at WS-01
    (make-instance of Item (object-id [em-02-01]) (location-id [workstation-01]) (quantity 1))

    ; Common shelf container ER-02-01 in location CB-01
    (make-instance of Item (object-id [er-02-01]) (location-id [conveyor_belt-01]) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver 2 machined cover plates AX-07 into file-card box EM-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-07]) (container-id [em-02-01]) (quantity-requested 1))

    ; Deliver 2 unusable cover plates AX-08 into common shelf container ER-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-01]) (quantity-requested 1))
    (make-instance of Order (status OFFERED) (object-id [ax-08]) (container-id [er-02-01]) (quantity-requested 1))
  )
)

(defmessage-handler TaskBenchmark2 handle-feedback (?pb-msg ?time ?name ?team)
  (if (and
       (pb-has-field ?pb-msg "after_receiving")
       (pb-has-field ?pb-msg "after_drilling"))
   then
    (return CONTINUE)   ; TBM feedback is valid -> continue the benchmark
   else
    (return FINISH)     ; TBM feedback is invalid -> finish the benchmark
  )
)




(defmessage-handler TaskBenchmark3 setup (?time ?state-machine)
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


  (bind ?shelf-locations (create$ [shelf-01] [shelf-02] [shelf-03] [shelf-04]
        [shelf-05] [shelf-06] [shelf-07] [shelf-08] [shelf-09] [shelf-10]
        [shelf-11] [shelf-12]))

  ; Randomize a location for ER-02-01
  (bind ?er-02-01-location (pick-random$ ?shelf-locations))

  ; The location of the assembly aid tray should should not be reused
  (bind ?shelf-locations (delete-member$ ?shelf-locations ?er-02-01-location))

  ; Inventory
  (slot-insert$ [inventory] items 1
    ;;;;;;;;;;;;;;;;
    ; Foam container
    ;;;;;;;;;;;;;;;;

    ; Foam container ER-02-01 at random location
    (make-instance of Item (object-id [er-02-01]) (location-id ?er-02-01-location) (quantity 1))

    ;;;;;;;;;;
    ; Objects
    ;;;;;;;;;;

    ; 1 bearing box AX-01 at random location
    (make-instance of Item (object-id [ax-01]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))

    ; 1 bearing AX-02 at random location
    (make-instance of Item (object-id [ax-02]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))

    ; 1 axis AX-03 at random location
    (make-instance of Item (object-id [ax-03]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))

    ; 1 shaft nut AX-04 at random location
    (make-instance of Item (object-id [ax-04]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))

    ; 1 distance tube AX-05 at random location
    (make-instance of Item (object-id [ax-05]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))

    ; 1 motor with gear box AX-09 at random location
    (make-instance of Item (object-id [ax-09]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
	    ; Deliver foam container ER-02-01 to workstation WS-06
    (make-instance of Order (status OFFERED) (object-id [er-02-01]) (destination-id [workstation-06]))

    ; Deliver 1 bearing box AX-01 into foam container ER-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-01]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 bearing AX-02 into foam container ER-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-02]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 axis AX-03 into foam container ER-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-03]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 shaft nut AX-04 into foam container ER-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-04]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 distance tube AX-05 into foam container ER-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-05]) (container-id [er-02-01]) (quantity-requested 1))

    ; Deliver 1 motor with gear box AX-09 into foam container ER-02-01
    (make-instance of Order (status OFFERED) (object-id [ax-09]) (container-id [er-02-01]) (quantity-requested 1))
  )
)

(defmessage-handler TaskBenchmark2 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)




(defrule init-tbm
  (init)
  ?bm <- (object (is-a Benchmark))
  =>
  (make-instance [TBM1] of TaskBenchmark1 (type TBM) (type-id 1) (description "Prepare Assembly Aid Tray for Force Fitting"))
  (make-instance [TBM2] of TaskBenchmark2 (type TBM) (type-id 2) (description "Plate Drilling"))
  (make-instance [TBM3] of TaskBenchmark3 (type TBM) (type-id 3) (description "Fill a Box with Parts for Manual Assembly"))

  (slot-insert$ ?bm registered-scenarios 1 [TBM1])
  (slot-insert$ ?bm registered-scenarios 1 [TBM2])
  (slot-insert$ ?bm registered-scenarios 1 [TBM3])
)
