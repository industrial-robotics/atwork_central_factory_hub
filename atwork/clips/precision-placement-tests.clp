;---------------------------------------------------------------------------
;  precision-placement-tests.clp - AtWork RefBox CLIPS - PPTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass PrecisionPlacementTest1 (is-a BenchmarkScenario) (role concrete))

(defmessage-handler PrecisionPlacementTest1 setup (?time ?state-machine)
  (make-instance [prep-timeup-state] of TimeoutState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*PPT-PREPARATION-TIME*))
  (make-instance [prep-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [exec-stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [exec-running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*PPT-EXECUTION-TIME*))
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

  (bind ?precision-objects (create$
    [F20_20_B] [F20_20_G] [S40_40_B] [S40_40_G] [M20_100] [M20] [M30] [R20] [BEARING_BOX]
    [BEARING] [AXIS] [DISTANCE_TUBE] [MOTOR]
  ))

  (bind ?workstation-locations (create$
        [workstation-01] [workstation-02] [workstation-03] [workstation-04]
        [workstation-05] [workstation-06] [workstation-07] [workstation-08]
        [workstation-09]
  ))

  ; Randomize a location for source
  (bind ?source-location (pick-random$ ?workstation-locations))
  ; The location of the assembly aid tray should should not be reused
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location))
  ; Randomize a location for destination
  (bind ?destination-location [precision-01])

  (bind ?item-1 (pick-random$ ?precision-objects))
  (bind ?item-2 (pick-random$ ?precision-objects))
  (bind ?item-3 (pick-random$ ?precision-objects))
  (bind ?item-4 (pick-random$ ?precision-objects))
  (bind ?item-5 (pick-random$ ?precision-objects))

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
    ; 1st Placement Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-1)
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)
    )))
    ; 2nd Placement Task
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
    ; 5th Placement Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-5)
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)
    )))
  )
)

(defmessage-handler PrecisionPlacementTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

(defrule init-ppt
  (init)
  ?ppt <- (object (is-a Benchmark))
  =>
  (make-instance [PPT1] of PrecisionPlacementTest1 (type PPT) (type-id 1) (description "Precision Placement Test 1"))

  (slot-insert$ ?ppt registered-scenarios 1 [PPT1])
)
