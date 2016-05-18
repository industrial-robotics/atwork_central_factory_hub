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

;; BTT 2

(defclass BasicTransportationTest2 (is-a BenchmarkScenario) (role concrete))

(defmessage-handler BasicTransportationTest2 setup (?time ?state-machine)
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

  (bind ?manipulation-robocup-objects ?*ROBOCUP-OBJECTS*)
  (bind ?manipulation-rockin-objects ?*ROCKIN-OBJECTS*)

  (bind ?item-1 (pick-random$ ?manipulation-robocup-objects))
  (bind ?item-2 (pick-random$ ?manipulation-robocup-objects))
  (bind ?item-3 (pick-random$ ?manipulation-rockin-objects))
  (bind ?item-4 (pick-random$ ?manipulation-rockin-objects))
  (bind ?item-5 (pick-random$ ?manipulation-rockin-objects))

  (bind ?workstation-locations ?*WORKSTATION-10CM-LOCATIONS*)

  ; Randomize first source location
  (bind ?source-location-1 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-1))
  ; Randomize second source location
  (bind ?source-location-2 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-2))

  (bind ?destination-location-1 (pick-random$ ?*SHELF-LOCATIONS*))
  (bind ?destination-location-2 [conveyorbelt-02])
  (bind ?destination-location-3 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?destination-location-3))
  (bind ?destination-location-4 (pick-random$ ?workstation-locations))


  ; Inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?item-1) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-2) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-3) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-4) (location-id ?source-location-2))
    (make-instance of Item (object-id ?item-5) (location-id ?source-location-2))
    (make-instance of Item (object-id [CONTAINER_B]) (location-id ?destination-location-4))
    (make-instance of Item (object-id [CONTAINER_R]) (location-id ?destination-location-4))
  )

  ; Tasks
  (slot-insert$ [task-info] tasks 1
    ; 1st Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-1)
        (quantity-requested 1)
        (destination-id ?destination-location-1)
        (source-id ?source-location-1)
    )))
    ; 2nd Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-2)
        (quantity-requested 1)
        (destination-id ?destination-location-2)
        (source-id ?source-location-1)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-3)
        (quantity-requested 1)
        (destination-id ?destination-location-3)
        (source-id ?source-location-1)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-4)
        (container-id [CONTAINER_B])
        (quantity-requested 1)
        (destination-id ?destination-location-4)
        (source-id ?source-location-2)
    )))
    ; 5th Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-5)
        (container-id [CONTAINER_R])
        (quantity-requested 1)
        (destination-id ?destination-location-4)
        (source-id ?source-location-2)
    )))
  )
)

(defmessage-handler BasicTransportationTest2 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)
(defrule init-btt
  (init)
  ?btt <- (object (is-a Benchmark))
  =>
  (make-instance [BTT1] of BasicTransportationTest1 (type BTT) (type-id 1) (description "Basic Transportation Test 1"))
  (make-instance [BTT2] of BasicTransportationTest2 (type BTT) (type-id 2) (description "Basic Transportation Test 2"))

  (slot-insert$ ?btt registered-scenarios 1 [BTT1])
  (slot-insert$ ?btt registered-scenarios 1 [BTT2])
)
