;---------------------------------------------------------------------------
;  basic-navigation-tests.clp - AtWork RefBox CLIPS - BNTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicNavigationTest1 (is-a BenchmarkScenario) (role concrete))

(defmessage-handler BasicNavigationTest1 setup (?time ?state-machine)
  (make-instance [prep-timeup-state] of TimeoutState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*BNT-PREPARATION-TIME*))
  (make-instance [prep-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [exec-stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [exec-running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BNT-EXECUTION-TIME*))
  (make-instance [exec-paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [exec-finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [prep-stopped-state]    add-transition START           [prep-running-state])
  (send [prep-running-state]    add-transition PAUSE           [prep-paused-state])
  (send [prep-running-state]    add-transition STOP            [exec-stopped-state])
  (send [prep-running-state]    add-transition TIMEOUT         [prep-timeup-state])
  (send [prep-running-state]    add-transition FINISH          [prep-timeup-state])
  (send [prep-paused-state]     add-transition START           [prep-running-state])
  (send [prep-paused-state]     add-transition STOP            [exec-stopped-state])

  (send [prep-timeup-state]     add-transition START         [exec-running-state])

  (send [exec-stopped-state]    add-transition START           [exec-running-state])
  (send [exec-running-state]    add-transition PAUSE           [exec-paused-state])
  (send [exec-running-state]    add-transition STOP            [exec-finished-state])
  (send [exec-running-state]    add-transition TIMEOUT         [exec-finished-state])
  (send [exec-running-state]    add-transition FINISH          [exec-finished-state])
  (send [exec-paused-state]     add-transition START           [exec-running-state])
  (send [exec-paused-state]     add-transition STOP            [exec-stopped-state])

  (make-instance ?state-machine of StateMachine
    (current-state [prep-stopped-state])
    (states
      [prep-stopped-state] [prep-running-state] [prep-paused-state] [prep-finished-state]
      [exec-stopped-state] [exec-running-state] [exec-paused-state] [exec-finished-state]
    )
  )


  (bind ?shelf-locations (create$
        [shelf-01] [shelf-02]
  ))
  (bind ?workstation-locations (create$
        [workstation-01] [workstation-02] [workstation-03] [workstation-04]
        [workstation-05] [workstation-06] [workstation-07] [workstation-08]
        [workstation-09] [workstation-10] [workstation-11] [workstation-12]
        [precision-01]
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

  ; Pick random location and remove from location pool
  (bind ?location-1 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-1))
  ; Pick random location and remove from location pool
  (bind ?location-2 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-2))
  ; Pick random location and remove from location pool
  (bind ?location-3 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-3))
  ; Pick random location and remove from location pool
  (bind ?location-4 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-4))
  ; Pick random location and remove from location pool
  (bind ?location-5 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-5))
  ; Pick random location and remove from location pool
  (bind ?location-6 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-6))
  ; Pick random location and remove from location pool
  (bind ?location-7 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-7))
  ; Pick random location and remove from location pool
  (bind ?location-8 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-8))
  ; Pick random location and remove from location pool
  (bind ?location-9 (pick-random$ ?navigation-locations))
  (bind ?navigation-locations (delete-member$ ?navigation-locations ?location-9))

  ; Tasks
  (slot-insert$ [task-info] tasks 1
    ; 1st Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-1)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 2nd Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-2)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 3rd Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-3)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 4th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-4)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 5th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-5)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 6th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-6)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 7th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-7)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 8th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-8)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 9th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id ?location-9)
        (wait-time 3)
        (orientation (pick-random$ ?navigation-directions))
    )))
  )
)

(defmessage-handler BasicNavigationTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)




(defrule init-bnt
  (init)
  ?bnt <- (object (is-a Benchmark))
  =>
  (make-instance [BNT1] of BasicNavigationTest1 (type BNT) (type-id 1) (description "Basic Navigation Test 1"))

  (slot-insert$ ?bnt registered-scenarios 1 [BNT1])
)
