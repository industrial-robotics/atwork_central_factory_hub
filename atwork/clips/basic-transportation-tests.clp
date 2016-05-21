;---------------------------------------------------------------------------
;  basic-transportation-tests.clp - AtWork RefBox CLIPS - BTTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicTransportationTest (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler BasicTransportationTest setup (?time ?state-machine)
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
)

(defmessage-handler BasicTransportationTest handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

(defclass BasicTransportationTest1 (is-a BasicTransportationTest) (role concrete))

(defmessage-handler BasicTransportationTest1 setup (?time ?state-machine)
  (call-next-handler)

  (bind ?transportation-objects ?*ROBOCUP-OBJECTS*)

  (bind ?decoy-objects (create$ ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*))

  (bind ?workstation-locations ?*WORKSTATION-10CM-LOCATIONS*)

  ; Source locations must exist before adding to it.
  (bind ?source-locations (create$ ))
  ; Draw 2 source locations in a loop, this counter could be configured in the future.
  (loop-for-count (?counter 1 2) do
    (bind ?location  (pick-random$ ?workstation-locations))
    (bind ?source-locations (create$ ?source-locations ?location))
    (bind ?workstation-locations (delete-member$ ?workstation-locations ?location))
  )

  (loop-for-count (?counter 1 5) do
    (bind ?item (pick-random$ ?transportation-objects))
    (bind ?decoy-objects (delete-member$ ?decoy-objects ?item))
    ; Get a source location from source locations.
    ; Try to evenly distribute source locations.
    (bind ?source-location (nth$ (+ 1 (mod ?counter (length ?source-locations))) ?source-locations))

    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item)
                             (location-id ?source-location))
    )

    ; Insert a task with random destination into tasks
    (slot-insert$ [task-info] tasks 1
      ; 1st Transportation Task
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id ?item)
          (quantity-requested 1)
          (destination-id (pick-random$ ?workstation-locations))
          (source-id ?source-location))))
    )
  )
  (loop-for-count (?counter 1 3) do
    ; Get a source location from source locations.
    ; Try to evenly distribute source locations.
    (bind ?source-location (nth$ (+ 1 (mod ?counter (length ?source-locations))) ?source-locations))
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id (pick-random$ ?decoy-objects)) (location-id ?source-location))
    )
  )
)


;; BTT 2

(defclass BasicTransportationTest2 (is-a BasicTransportationTest) (role concrete))

(defmessage-handler BasicTransportationTest2 setup (?time ?state-machine)
  (call-next-handler)

  (bind ?manipulation-robocup-objects ?*ROBOCUP-OBJECTS*)
  (bind ?manipulation-rockin-objects ?*ROCKIN-OBJECTS*)

  (bind ?manipulation-objects (create$
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-rockin-objects)
    (pick-random$ ?manipulation-rockin-objects)
    (pick-random$ ?manipulation-rockin-objects)
  ))

  (bind ?manipulation-objects (randomize$ ?manipulation-objects))

  (bind ?item-1 (nth$ 1 ?manipulation-objects))
  (bind ?item-2 (nth$ 2 ?manipulation-objects))
  (bind ?item-3 (nth$ 3 ?manipulation-objects))
  (bind ?item-4 (nth$ 4 ?manipulation-objects))
  (bind ?item-5 (nth$ 5 ?manipulation-objects))

  (bind ?workstation-locations ?*WORKSTATION-10CM-LOCATIONS*)

  ; Randomize first source location
  (bind ?source-location-1 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-1))
  ; Randomize second source location
  (bind ?source-location-2 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-2))

  (bind ?destination-location-1 (pick-random$ ?*SHELF-LOCATIONS*))
  (bind ?destination-location-2 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?destination-location-2))
  (bind ?destination-location-3 (pick-random$ ?workstation-locations))
  (bind ?destination-location-4 [conveyorbelt-02])

  (bind ?destination-locations (create$
    ?destination-location-1
    ?destination-location-2
    ?destination-location-3
    ; Do not include rotating table
    ;?destination-location-4
  ))

  (bind ?destination-locations (randomize$ ?destination-locations))

  (bind ?destination-location-1 (nth$ 1 ?destination-locations))
  (bind ?destination-location-2 (nth$ 2 ?destination-locations))
  (bind ?destination-location-3 (nth$ 3 ?destination-locations))
  ;(bind ?destination-location-4 (nth$ 4 ?destination-locations))

  ; Inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?item-1) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-2) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-3) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-4) (location-id ?source-location-2))
    (make-instance of Item (object-id ?item-5) (location-id ?source-location-2))
    (make-instance of Item (object-id [CONTAINER_B]) (location-id ?destination-location-1))
    (make-instance of Item (object-id [CONTAINER_R]) (location-id ?destination-location-1))
  )

  ; Tasks
  (slot-insert$ [task-info] tasks 1
    ; 1st Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-1)
        (quantity-requested 1)
        (destination-id ?destination-location-4)
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
        (destination-id ?destination-location-1)
        (source-id ?source-location-2)
    )))
    ; 5th Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-5)
        (container-id [CONTAINER_R])
        (quantity-requested 1)
        (destination-id ?destination-location-1)
        (source-id ?source-location-2)
    )))
  )
)

;; BTT 3

(defclass BasicTransportationTest3 (is-a BasicTransportationTest) (role concrete))

(defmessage-handler BasicTransportationTest3 setup (?time ?state-machine)
  (call-next-handler)

  (bind ?manipulation-robocup-objects ?*ROBOCUP-OBJECTS*)
  (bind ?manipulation-rockin-objects ?*ROCKIN-OBJECTS*)
  (bind ?manipulation-objects (create$
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-rockin-objects)
    (pick-random$ ?manipulation-rockin-objects)
    (pick-random$ ?manipulation-rockin-objects)
  ))

  (bind ?manipulation-objects (randomize$ ?manipulation-objects))

  (bind ?item-1 (nth$ 1 ?manipulation-objects))
  (bind ?item-2 (nth$ 2 ?manipulation-objects))
  (bind ?item-3 (nth$ 3 ?manipulation-objects))
  (bind ?item-4 (nth$ 4 ?manipulation-objects))
  (bind ?item-5 (nth$ 5 ?manipulation-objects))
  (bind ?item-6 (nth$ 6 ?manipulation-objects))
  (bind ?item-7 (nth$ 7 ?manipulation-objects))

  (bind ?workstation-locations (create$
    ?*WORKSTATION-0CM-LOCATIONS*
    ?*WORKSTATION-10CM-LOCATIONS*
    ?*WORKSTATION-15CM-LOCATIONS*
  ))
  (bind ?workstation-0cm-locations ?*WORKSTATION-0CM-LOCATIONS*)
  (bind ?workstation-10cm-locations ?*WORKSTATION-10CM-LOCATIONS*)
  (bind ?workstation-15cm-locations ?*WORKSTATION-15CM-LOCATIONS*)
  (bind ?decoy-objects (create$ ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*))

  ; Pick source locations
  ; Then remove picked locations from both overall set and hieght specific set
  ; pick first source location from 0 cm locations
  (bind ?source-location-1 (pick-random$ ?workstation-0cm-locations))
  (bind ?workstation-0cm-locations (delete-member$ ?workstation-0cm-locations ?source-location-1))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-1))
  ; pick second source location from 10 cm locations
  (bind ?source-location-2 (pick-random$ ?workstation-0cm-locations))
  (bind ?workstation-10cm-locations (delete-member$ ?workstation-10cm-locations ?source-location-2))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-2))
  ; pick third source location from 15 cm locations
  (bind ?source-location-3 (pick-random$ ?workstation-15cm-locations))
  (bind ?workstation-15cm-locations (delete-member$ ?workstation-15cm-locations ?source-location-3))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-3))
  ; Randomize firth source location from any height
  (bind ?source-location-4 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?source-location-4))

  ; Create list of all source locations
  (bind ?source-locations (create$
    ?source-location-1
    ?source-location-2
    ?source-location-3
    ?source-location-4
  ))
  ; Shuffle list of source locations
  (bind ?source-locations (randomize$ ?source-locations))
  ; Take new source locations from shuffled order
  (bind ?source-location-1 (nth$ 1 ?source-locations))
  (bind ?source-location-2 (nth$ 2 ?source-locations))
  (bind ?source-location-3 (nth$ 3 ?source-locations))
  (bind ?source-location-4 (nth$ 4 ?source-locations))

  ; Draw destination locations according to task instances
  (bind ?destination-location-1 (pick-random$  ?workstation-0cm-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?destination-location-1))
  (bind ?destination-location-2 (pick-random$  ?workstation-10cm-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?destination-location-2))
  (bind ?destination-location-3 (pick-random$  ?workstation-15cm-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?destination-location-3))
  (bind ?destination-location-4 (pick-random$ ?workstation-locations))
  (bind ?workstation-locations (delete-member$ ?workstation-locations ?destination-location-4))
  (bind ?destination-location-5 [conveyorbelt-02])

  ; Create list of destination locations from drawn destination locations
  (bind ?destination-locations (create$
    ?destination-location-1
    ?destination-location-2
    ?destination-location-3
    ?destination-location-4
    ; Do not include rotating table, we need to know this location
    ;?destination-location-5
  ))
  ; Shuffle list of destination locations
  (bind ?destination-locations (randomize$ ?destination-locations))
  ; Take new locations from shuffled locations
  (bind ?destination-location-1 (nth$ 1 ?destination-locations))
  (bind ?destination-location-2 (nth$ 2 ?destination-locations))
  (bind ?destination-location-3 (nth$ 3 ?destination-locations))
  (bind ?destination-location-4 (nth$ 4 ?destination-locations))
  ; Do not include rotating table, we need to know this location
  ;(bind ?destination-location-5 (nth$ 5 ?destination-locations))


  ; Remove items from decoy set
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-1))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-2))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-3))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-4))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-5))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-6))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-7))
  ; Pick 3 random decoy objects
  (bind ?decoy-1 (pick-random$ ?decoy-objects))
  (bind ?decoy-2 (pick-random$ ?decoy-objects))
  (bind ?decoy-3 (pick-random$ ?decoy-objects))

  ; Inventory
  (slot-insert$ [inventory] items 1
    ; source location 1
    (make-instance of Item (object-id ?item-1) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-2) (location-id ?source-location-1))
    (make-instance of Item (object-id ?decoy-1) (location-id ?source-location-1))
    ; source location 2
    (make-instance of Item (object-id ?item-3) (location-id ?source-location-2))
    (make-instance of Item (object-id ?item-4) (location-id ?source-location-2))
    (make-instance of Item (object-id ?decoy-2) (location-id ?source-location-2))
    ; source location 3
    (make-instance of Item (object-id ?item-5) (location-id ?source-location-3))
    (make-instance of Item (object-id ?item-6) (location-id ?source-location-3))
    ; source location 4
    (make-instance of Item (object-id ?item-7) (location-id ?source-location-4))
    (make-instance of Item (object-id ?decoy-3) (location-id ?source-location-4))
    ; destination location 4
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
    ; 3rd Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-3)
        (quantity-requested 1)
        (destination-id ?destination-location-3)
        (source-id ?source-location-2)
    )))
    ; 4th Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-4)
        (quantity-requested 1)
        (destination-id ?destination-location-3)
        (source-id ?source-location-2)
    )))
    ; 5th Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-5)
        (quantity-requested 1)
        (destination-id ?destination-location-5)
        (source-id ?source-location-3)
    )))
    ; 6th Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-6)
        (container-id [CONTAINER_R])
        (quantity-requested 1)
        (destination-id ?destination-location-4)
        (source-id ?source-location-3)
    )))
    ; 7th Transportation Task
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-7)
        (container-id [CONTAINER_R])
        (quantity-requested 1)
        (destination-id ?destination-location-4)
        (source-id ?source-location-4)
    )))
  )

)

;; END BTT3

(defrule init-btt
  (init)
  ?btt <- (object (is-a Benchmark))
  =>
  (make-instance [BTT1] of BasicTransportationTest1 (type BTT) (type-id 1) (description "Basic Transportation Test 1"))
  (make-instance [BTT2] of BasicTransportationTest2 (type BTT) (type-id 2) (description "Basic Transportation Test 2"))
  (make-instance [BTT3] of BasicTransportationTest3 (type BTT) (type-id 3) (description "Basic Transportation Test 3"))

  (slot-insert$ ?btt registered-scenarios 1 [BTT1])
  (slot-insert$ ?btt registered-scenarios 1 [BTT2])
  (slot-insert$ ?btt registered-scenarios 1 [BTT3])
)
