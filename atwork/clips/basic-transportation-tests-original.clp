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

(defmessage-handler BasicTransportationTest1 generate ()
  (printout t "Generating new BasicTransportationTest1" crlf)

  (bind ?transportation-objects ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*)
  (bind ?decoy-objects (create$ ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*))
  (bind ?workstation-locations    (create$ ?*WORKSTATION-10CM-LOCATIONS* ))
  ; Source locations must exist before adding to it.
  (bind ?source-locations (create$ ))
  ; Draw 2 source locations in a loop, this counter could be configured in the future.
  (loop-for-count (?counter 1 3) do
    (bind ?location  (pick-random$ ?workstation-locations))
    (bind ?source-locations (create$ ?source-locations ?location))
    ;(bind ?workstation-locations (delete-member$ ?workstation-locations ?location))
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

(defmessage-handler BasicTransportationTest2 generate ()
  (printout t "Generating new BasicTransportationTest2" crlf)

  (bind ?objects          ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*)
  (bind ?decoy-objects          ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*)
;  (bind ?workstation-locations    (create$ ?*WORKSTATION-10CM-LOCATIONS* ?*WORKSTATION-5CM-LOCATIONS* ?*WORKSTATION-15CM-LOCATIONS* ?*WORKSTATION-0CM-LOCATIONS*))
  (bind  ?workstation-locations (create$ [workstation-07] [workstation-02] [workstation-04] [workstation-10] [workstation-11]))
  (bind ?shelf-locations          ?*SHELF-LOCATIONS*)
  (bind ?rotating-table-locations ?*ROTATING-TABLE-LOCATIONS*)

  ; create empty list of objects for transportation
  (bind ?transportation-objects (create$ ))
  ; pick objects from the robocup set
  (loop-for-count 6
    (bind ?item (pick-random$ ?objects))
    (bind ?transportation-objects (create$ ?transportation-objects
      ?item))
    (bind ?decoy-objects (delete-member$ ?decoy-objects ?item))
  )
  ; shuffle the objects
  (bind ?transportation-objects (randomize$ ?transportation-objects))

  ; Source locations must exist before adding to it.
  (bind ?source-locations (create$ ))
  ; Draw 2 source locations in a loop, this counter could be configured in the future.
  (loop-for-count 3 do
    (bind ?location  (pick-random$ ?workstation-locations))
    (bind ?source-locations (create$ ?source-locations ?location))
;    (bind ?workstation-locations (delete-member$ ?workstation-locations ?location))
  )

  ; Create a list for desintation locations start with one shelf
  ;(bind ?location (pick-random$ ?shelf-locations))
  (bind ?destination-locations (create$))
  (loop-for-count 3 do
    (bind ?location (pick-random$ ?workstation-locations))
    (bind ?destination-locations (create$ ?destination-locations ?location))
    (bind ?workstation-locations (delete-member$ ?workstation-locations ?location))
  )
  ; shuffle the list of destination locations
  (bind ?destination-locations (randomize$ ?destination-locations))

  ; Need 5 transportation tasks
  ; but on the last we create 2... one for each container. and the rotating table is done later.
  (bind ?last 5)
  (loop-for-count (?counter 1 ?last) do
    (bind ?item (nth$ ?counter ?transportation-objects))
    ; Get a source location from source locations.
    ; Try to evenly distribute source locations.
    (bind ?source-location (nth$ (+ 1 (mod ?counter (length ?source-locations))) ?source-locations))
    (bind ?destination-location (nth$ (+ 1 (mod ?counter (length ?destination-locations))) ?destination-locations))
    ; Inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item)
                             (location-id ?source-location))
    )
    ; Task
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
  ; One object must be placed on the rotating table
  (bind ?last (+ 1 ?last))
  (bind ?item (nth$ ?last ?transportation-objects))
  (bind ?source-location (nth$ (+ 1 (mod ?last (length ?source-locations))) ?source-locations))
  (bind ?destination-location (pick-random$ ?rotating-table-locations))
  ; Inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?item)
                           (location-id ?source-location))
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
  (loop-for-count (?counter 1 3) do
    ; Get a source location from source locations.
    ; Try to evenly distribute source locations.
    (bind ?source-location (nth$ (+ 1 (mod ?counter (length ?source-locations))) ?source-locations))
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id (pick-random$ ?decoy-objects)) (location-id ?source-location))
    )
  )
)

;; BTT 3

(defclass BasicTransportationTest3 (is-a BasicTransportationTest) (role concrete))

(defmessage-handler BasicTransportationTest3 generate ()
  (printout t "Generating new BasicTransportationTest3" crlf)

  ; Get Globals
  (bind ?objects                ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*)
  (bind ?shelf-locations                ?*SHELF-LOCATIONS*)
  (bind ?workstation-locations (create$ ?*WORKSTATION-10CM-LOCATIONS*))
  (bind ?decoy-objects (create$         ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*))

  ; create empty list of objects for transportation
  (bind ?transportation-objects (create$ ))
  ; pick 6 objects from the robocup set
  (loop-for-count 6 do
    (bind ?item (pick-random$ ?objects))
    (bind ?transportation-objects (create$ ?transportation-objects ?item))
    (bind ?decoy-objects (delete-member$ ?decoy-objects ?item))
  )
  ; shuffle the objects
  (bind ?transportation-objects (randomize$ ?transportation-objects))

  ;  two Source Locations
  (bind ?source-locations (create$ ))
  (loop-for-count (?counter 1 2) do
    (bind ?location (pick-random$ ?workstation-locations))
    (bind ?source-locations (create$ ?source-locations ?location))
    (bind ?workstation-locations (delete-member$ ?workstation-locations ?location))
  )
  ;add shelf for pick
  (bind ?shelf-location (pick-random$ ?shelf-locations))
  ;(bind ?source-locations (create$ ?source-locations ?location))
  (bind ?shelf-locations (delete-member$ ?shelf-locations ?location))
  

; Shuffle list of source locations
  (bind ?source-locations (randomize$ ?source-locations))

  ; Destination Locations
  (bind ?destination-locations (create$ ))
  (bind ?location (pick-random$ ?workstation-locations))
  (bind ?destination-locations (create$ ?destination-locations ?location))

  ; Randomly pick last source location from any height
  ;(bind ?location (pick-random$ ?workstation-locations))
  ;(bind ?destination-locations (create$ ?destination-locations ?location))

  ;Create first Transport from shelf to source1
  (bind ?item (pick-random$ ?transportation-objects))
  (bind ?transportation-objects (delete-member$ ?transportation-objects ?item))
  ; Get a source location from source locations.
  ; Try to evenly distribute source locations.
  ;(bind ?source-location (nth$ 1 ?source-locations))
  (bind ?destination-location (nth$ 1 ?source-locations))
    ; Inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?item)
                           (location-id ?shelf-location))
  )

  ;Create first Transport from shelf to source2
  (bind ?item (pick-random$ ?transportation-objects))
  (bind ?transportation-objects (delete-member$ ?transportation-objects ?item))
  ; Get a source location from source locations.
  ; Try to evenly distribute source locations.
  ;(bind ?source-location (nth$ 1 ?source-locations))

    ; Inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?item)
                           (location-id ?shelf-location))
  )
  ; Task
  (slot-insert$ [task-info] tasks 1
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
      (object-id ?item)
      (quantity-requested 1)
      (destination-id ?destination-location)
      (source-id ?shelf-location))))
   )
   (make-instance of Item (object-id [CONTAINER_B]) (location-id ?destination-location))
   (make-instance of Item (object-id [CONTAINER_R]) (location-id ?destination-location))
  ;Put 4 Objects into container
  (bind ?last 2)
  (loop-for-count (?counter 1 ?last) do
    ; Get a source location from source locations.
    ; Try to evenly distribute source locations.
    (bind ?source-location (nth$ (+ 1 (mod ?counter (length ?source-locations))) ?source-locations))
    (bind ?destination-location (nth$ 1 ?destination-locations))

    (bind ?item (pick-random$ ?transportation-objects))
    (bind ?transportation-objects (delete-member$ ?transportation-objects ?item))
    ; Inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item)
                             (location-id ?source-location))
    )
    ;item for red container

    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
        (object-id ?item)
        (container-id [CONTAINER_R])
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)))))

    (bind ?item (pick-random$ ?transportation-objects))
    (bind ?transportation-objects (delete-member$ ?transportation-objects ?item))
    ; Inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item)
                             (location-id ?source-location))
    )
    ;item for blue container

    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
        (object-id ?item)
        (container-id [CONTAINER_B])
        (quantity-requested 1)
        (destination-id ?destination-location)
        (source-id ?source-location)))))
  )

  ; One object must be placed on the shelf
  (bind ?last (+ 1 ?last))
  (bind ?item (pick-random$ ?transportation-objects))
  (bind ?source-location (nth$ (+ 1 (mod ?last (length ?source-locations))) ?source-locations))
  (bind ?destination-location (pick-random$ ?shelf-locations))
  ; Inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?item)
                           (location-id ?source-location))
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

  (loop-for-count (?counter 1 3) do
    ; Get a source location from source locations.
    ; Try to evenly distribute source locations.
    (bind ?source-location (nth$ (+ 1 (mod ?counter (length ?source-locations))) ?source-locations))
    (slot-insert$ [inventory] items 1
     (make-instance of Item (object-id (pick-random$ ?decoy-objects)) (location-id ?source-location))
    )
  )
)

;; END BTT3


;; BEGIN AST
(defclass ArbSurfaceTest (is-a BasicTransportationTest) (role concrete))

(defmessage-handler ArbSurfaceTest generate ()
  (printout t "Generating new ArbSurfaceTest" crlf)

  (bind ?transportation-objects ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*)
  (bind ?decoy-objects (create$ ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*))
  (bind ?workstation-locations    (create$ ?*WORKSTATION-10CM-LOCATIONS*))
  (bind ?destination (nth$ 1 ?*WORKSTATION-15CM-LOCATIONS*))
  ; Source locations must exist before adding to it.
  (bind ?source-locations (create$ ))
  ; Draw 2 source locations in a loop, this counter could be configured in the future.
  (loop-for-count (?counter 1 3) do
    (bind ?location  (pick-random$ ?workstation-locations))
    (bind ?source-locations (create$ ?source-locations ?location))
    (bind ?workstation-locations (delete-member$ ?workstation-locations ?location))
  )

  (loop-for-count (?counter 1 3) do
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
          (destination-id ?destination)
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

;; END AST

(defrule init-btt
  (init)
  ?btt <- (object (is-a Benchmark))
  =>
  (make-instance [BTT1] of BasicTransportationTest1 (type BTT) (type-id 1) (description "Basic Transportation Test 1"))
  (make-instance [BTT2] of BasicTransportationTest2 (type BTT) (type-id 2) (description "Basic Transportation Test 2"))
  (make-instance [BTT3] of BasicTransportationTest3 (type BTT) (type-id 3) (description "Basic Transportation Test 3"))
  (make-instance [AST] of ArbSurfaceTest (type BTT) (type-id 4) (description "Arbitrary Surface Test"))

  (slot-insert$ ?btt registered-scenarios 1 [BTT1])
  (slot-insert$ ?btt registered-scenarios 1 [BTT2])
  (slot-insert$ ?btt registered-scenarios 1 [BTT3])
  (slot-insert$ ?btt registered-scenarios 1 [AST])
)
