;---------------------------------------------------------------------------
;  basic-transportation-tests.clp - AtWork RefBox CLIPS - BTTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicTransportationTest1 (is-a BenchmarkScenario) (role concrete))

(defmessage-handler BasicTransportationTest1 setup (?time ?state-machine)
  (make-instance [prep-timeup-state] of TimeoutState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*BTT-PREPARATION-TIME*))
  (make-instance [prep-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [exec-stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [exec-running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BTT-EXECUTION-TIME*))
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

  (bind ?transportation-objects ?*ROBOCUP-OBJECTS*)

  (bind ?decoy-objects (create$ ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*))

  (bind ?workstation-locations ?*WORKSTATION-10CM-LOCATIONS*)

  ; Randomize first source location
  (bind ?source-location-1 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-1))
  ; Randomize second source location
  (bind ?source-location-2 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-2))

  (bind ?item-1 (pick-random$ ?transportation-objects))
  (bind ?item-2 (pick-random$ ?transportation-objects))
  (bind ?item-3 (pick-random$ ?transportation-objects))
  (bind ?item-4 (pick-random$ ?transportation-objects))
  (bind ?item-5 (pick-random$ ?transportation-objects))

  ; Inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?item-1) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-2) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-3) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-4) (location-id ?source-location-2))
    (make-instance of Item (object-id ?item-5) (location-id ?source-location-2))
  )

  ; Tasks
  (slot-insert$ [task-info] tasks 1
    ; 1st Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-1)
        (quantity-requested 1)
        (destination-id (pick-random$ ?workstation-locations))
        (source-id ?source-location-1)
    )))
    ; 2nd Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-2)
        (quantity-requested 1)
        (destination-id (pick-random$ ?workstation-locations))
        (source-id ?source-location-1)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-3)
        (quantity-requested 1)
        (destination-id (pick-random$ ?workstation-locations))
        (source-id ?source-location-1)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-4)
        (quantity-requested 1)
        (destination-id (pick-random$ ?workstation-locations))
        (source-id ?source-location-2)
    )))
    ; 5th Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-5)
        (quantity-requested 1)
        (destination-id (pick-random$ ?workstation-locations))
        (source-id ?source-location-2)
    )))
  )
)

(defmessage-handler BasicTransportationTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

(defrule init-btt
  (init)
  ?btt <- (object (is-a Benchmark))
  =>
  (make-instance [BTT1] of BasicTransportationTest1 (type BTT) (type-id 1) (description "Basic Transportation Test 1"))

  (slot-insert$ ?btt registered-scenarios 1 [BTT1])
)
