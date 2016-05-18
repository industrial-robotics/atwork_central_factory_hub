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
        [shelf-01] [shelf-02] [shelf-03] [shelf-04]
  ))
  (bind ?workstation-locations (create$
        [workstation-01] [workstation-02] [workstation-03] [workstation-04]
        [workstation-05] [workstation-06] [workstation-07] [workstation-08]
        [workstation-09] [precision-01]
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

  ; Tasks
  (slot-insert$ [task-info] tasks 1
    ; 1st Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 2nd Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 3rd Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 4th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 5th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 6th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 7th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 8th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
        (orientation (pick-random$ ?navigation-directions))
    )))
    ; 9th Navigation Goal
    (make-instance of Task (status OFFERED) (task-type NAVIGATION)
      (navigation-task (make-instance of NavigationTask
        (location-id (pick-random$ ?navigation-locations))
        (wait-time 5)
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
