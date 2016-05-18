;---------------------------------------------------------------------------
;  basic-manipulation-tests.clp - AtWork RefBox CLIPS - BMTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicManipulationTest1 (is-a BenchmarkScenario) (role concrete))

(defmessage-handler BasicManipulationTest1 setup (?time ?state-machine)
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

  (bind ?manipulation-robocup-objects (create$
    [F20_20_B] [F20_20_G] [S40_40_B] [S40_40_G] [M20_100] [M20] [M30] [R20]
  ))
  
  (bind ?manipulation-rockin-objects (create$
    [BEARING_BOX] [BEARING] [AXIS] [DISTANCE_TUBE] [MOTOR]
  ))

  ; set static location for source
  (bind ?source-location [workstation-05])
  ; set static location for destination
  (bind ?destination-location [workstation-06])

  (bind ?item-1 (pick-random$ ?manipulation-robocup-objects))
  (bind ?item-2 (pick-random$ ?manipulation-robocup-objects))
  (bind ?item-3 (pick-random$ ?manipulation-robocup-objects))
  (bind ?item-4 (pick-random$ ?manipulation-rockin-objects))
  (bind ?item-5 (pick-random$ ?manipulation-rockin-objects))

  ; Inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?item-1) (location-id ?source-location))
    (make-instance of Item (object-id ?item-2) (location-id ?source-location))
    (make-instance of Item (object-id ?item-3) (location-id ?source-location))
    (make-instance of Item (object-id ?item-4) (location-id ?source-location))
    (make-instance of Item (object-id ?item-5) (location-id ?source-location))
  )

  ; Tasks
  (slot-insert$ [task-info] tasks 1
    ; 1st Manipulation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-1)
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)
    )))
    ; 2nd Manipulation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-2)
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-3)
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-4)
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)
    )))
    ; 5th Manipulation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-5)
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)
    )))
  )
)

(defmessage-handler BasicManipulationTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

(defrule init-bmt
  (init)
  ?bmt <- (object (is-a Benchmark))
  =>
  (make-instance [BMT1] of BasicManipulationTest1 (type BMT) (type-id 1) (description "Basic Manipulation Test 1"))

  (slot-insert$ ?bmt registered-scenarios 1 [BMT1])
)
