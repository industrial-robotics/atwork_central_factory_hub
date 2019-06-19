;---------------------------------------------------------------------------
;  basic-navigation-tests.clp - AtWork RefBox CLIPS - BNTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

;; Generic BNT
(defclass BasicNavigationTest (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler BasicNavigationTest setup (?time ?state-machine)
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
)

(defmessage-handler BasicNavigationTest handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

;; BNT 1

(defclass BasicNavigationTest1 (is-a BasicNavigationTest) (role concrete))

(defmessage-handler BasicNavigationTest1 generate ()
  (printout t "Generating new BasicNavigationTest1" crlf)

  (bind ?navigation-locations (create$
        ?*WORKSTATION-0CM-LOCATIONS* ?*WORKSTATION-5CM-LOCATIONS*
        ?*WORKSTATION-10CM-LOCATIONS* ?*WORKSTATION-15CM-LOCATIONS*
        ?*SHELF-LOCATIONS* ?*ROTATING-TABLE-LOCATIONS*
        ?*PRECISION-LOCATIONS*
  ))
  (bind ?navigation-directions (create$
        NORTH EAST SOUTH WEST
  ))

  (loop-for-count 9
    ; Pick random location and remove from location pool
    (bind ?location (pick-random$ ?navigation-locations))
    (bind ?navigation-locations (delete-member$ ?navigation-locations ?location))

    ; Task
    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type NAVIGATION)
        (navigation-task (make-instance of NavigationTask
          (location-id ?location)
          (wait-time 3)
          (orientation (pick-random$ ?navigation-directions))))
      )
    )
  )
)


(defrule init-bnt
  (init)
  ?bnt <- (object (is-a Benchmark))
  =>
  (make-instance [BNT1] of BasicNavigationTest1 (type BNT) (type-id 1) (description "Basic Navigation Test 1"))

  (slot-insert$ ?bnt registered-scenarios 1 [BNT1])
)
