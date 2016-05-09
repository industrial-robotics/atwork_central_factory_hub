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


  (bind ?shelf-locations (create$
        [shelf-01] [shelf-02] [shelf-03] [shelf-04]
  ))
  (bind ?workstation-locations (create$
        [workstation-01] [workstation-02] [workstation-03] [workstation-04]
        [workstation-05] [workstation-06] [workstation-07] [workstation-08]
        [workstation-09]
  ))
  (bind ?conveyor-locations (create$
        [conveyorbelt-01] [conveyorbelt-02]
  ))
  (bind ?waypoint-locations (create$
        [waypoint-01] [waypoint-02] [waypoint-03] [waypoint-04] [waypoint-05]
        [waypoint-06] [waypoint-07] [waypoint-08] [waypoint-09] [waypoint-10]
        [waypoint-11] [waypoint-12] [waypoint-13] [waypoint-14] [waypoint-15]
  ))
  (bind ?navigation-locations (create$
        ?shelf-locations ?workstation-locations ?conveyor-locations ?waypoint-locations
  ))
  (bind ?navigation-directions (create$
        NORTH EAST SOUTH WEST
  ))

  ; Randomize a location for ER-02-01
  (bind ?er-02-01-location (pick-random$ ?shelf-locations))

  ; The location of the assembly aid tray should should not be reused
  (bind ?shelf-locations (delete-member$ ?shelf-locations ?er-02-01-location))

  ; Orders
  (slot-insert$ [order-info] orders 1
    ; 1st Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
    ; 2nd Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
    ; 3rd Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
    ; 4th Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
    ; 5th Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
    ; 6th Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
    ; 7th Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
    ; 8th Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
    ; 9th Navigation Goal
    (make-instance of Order (status OFFERED) (object-id [NAV_GOAL])
                            (destination-id (pick-random$ ?navigation-locations))
                            (wait-time 5) (orientation (pick-random$ ?navigation-directions)))
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
