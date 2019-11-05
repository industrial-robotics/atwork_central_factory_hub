;---------------------------------------------------------------------------
;  conveyor-belt-tests.clp - AtWork RefBox CLIPS - CBTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass ConveyorBeltTest (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler ConveyorBeltTest setup (?time ?state-machine)
  (make-instance [prep-timeup-state] of TimeoutState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*CBT-PREPARATION-TIME*))
  (make-instance [prep-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [exec-stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [exec-running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*CBT-EXECUTION-TIME*))
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

(defmessage-handler ConveyorBeltTest handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

;; CBT 1

(defclass ConveyorBeltTest1 (is-a ConveyorBeltTest) (role concrete))

(defmessage-handler ConveyorBeltTest1 generate ()
  (printout t "Generating new ConveyorBeltTest1" crlf)

  (bind ?manipulation-objects ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*)
  (bind ?decoy-objects          ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*)
  (bind ?source-location [conveyorbelt-01])
  (bind ?destination-location [robot])

  ; Create 3 items on the conveyor belt to be grasped
  (loop-for-count 3
    (bind ?item (pick-random$ ?manipulation-objects))
    (bind ?manipulation-objects (delete-member$ ?manipulation-objects ?item))
    (bind ?decoy-objects (delete-member$ ?decoy-objects ?item))

    ; Inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item) (location-id ?source-location))
    )

    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id (pick-random$ ?decoy-objects)) (location-id ?source-location))
    )

    ; Task
    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id ?item)
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location))))
    )
  )
)

;; CBT 2

(defclass ConveyorBeltTest2 (is-a ConveyorBeltTest) (role concrete))

(defmessage-handler ConveyorBeltTest2 generate ()
  (printout t "Generating new ConveyorBeltTest2" crlf)

  (bind ?manipulation-objects ?*ROBOCUP-OBJECTS*)
  (bind ?source-location [conveyorbelt-02])
  (bind ?destination-location [robot])

  ; Create 3 items on the conveyor belt to be grasped
  (loop-for-count 3
    (bind ?item (pick-random$ ?manipulation-objects))
    (bind ?manipulation-objects (delete-member$ ?manipulation-objects ?item))

    ; Inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item) (location-id ?source-location))
    )

    ; Task
    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id ?item)
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location))))
    )
  )
)


(defrule init-cbt
  (init)
  ?cbt <- (object (is-a Benchmark))
  =>
  (make-instance [CBT1] of ConveyorBeltTest1 (type CBT) (type-id 1) (description "Conveyor Belt Test 1"))
  (make-instance [CBT2] of ConveyorBeltTest2 (type CBT) (type-id 2) (description "Conveyor Belt Test 2"))

  (slot-insert$ ?cbt registered-scenarios 1 [CBT1])
  (slot-insert$ ?cbt registered-scenarios 1 [CBT2])
)
