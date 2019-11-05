;---------------------------------------------------------------------------
;  basic-manipulation-tests.clp - AtWork RefBox CLIPS - BMTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicManipulationTest (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler BasicManipulationTest setup (?time ?state-machine)
  (make-instance [prep-timeup-state] of TimeoutState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*BMT-PREPARATION-TIME*))
  (make-instance [prep-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [exec-stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [exec-running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BMT-EXECUTION-TIME*))
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

(defmessage-handler BasicManipulationTest handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

;; BMT 1

(defclass BasicManipulationTest1 (is-a BasicManipulationTest) (role concrete))

(defmessage-handler BasicManipulationTest1 generate ()
  (printout t "Generating new BasicManipulationTest1" crlf)

  (bind ?manipulation-robocup-objects ?*ROBOCUP-OBJECTS*)
  
  (bind ?manipulation-rockin-objects ?*ROCKIN-OBJECTS*)

  ; set static location for source
  (bind ?source-location [workstation-15])
  ; set static location for destination
  (bind ?destination-location [workstation-16])

  ; 3 RoboCup objects
  (loop-for-count 3
    ; Pick random RoboCup object
    (bind ?item (pick-random$ ?manipulation-robocup-objects))

    ; Add to inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item) (location-id ?source-location))
    )

    ; Manipulation Task
    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id ?item)
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
    )
  )

  ; 2 RoCKIn objects (TODO: Selected by team)
  (loop-for-count 2
    ; Pick random RoboCup object
    (bind ?item (pick-random$ ?manipulation-rockin-objects))

    ; Add to inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item) (location-id ?source-location))
    )

    ; Manipulation Task
    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id ?item)
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
    )
  )
)


(defrule init-bmt
  (init)
  ?bmt <- (object (is-a Benchmark))
  =>
  (make-instance [BMT1] of BasicManipulationTest1 (type BMT) (type-id 1) (description "Basic Manipulation Test 1"))

  (slot-insert$ ?bmt registered-scenarios 1 [BMT1])
)
