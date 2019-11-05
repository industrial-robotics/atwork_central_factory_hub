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
  ; set static location for source
  (bind ?source-location [workstation-07])
  ; set static location for destination
  (bind ?destination-location [workstation-06])

  ; 3 RoboCup objects
  ;(loop-for-count 3
    ; Pick random RoboCup object
 ;   (bind ?item (pick-random$ ?manipulation-robocup-objects))

    ; Add to inventory
    (slot-insert$ [inventory] items 5
      (make-instance of Item (object-id [M20_100]) (location-id [workstation-06]))
      (make-instance of Item (object-id [MOTOR])    (location-id [workstation-07]))
      (make-instance of Item (object-id [R20])      (location-id [workstation-06]))
      (make-instance of Item (object-id [M30]) (location-id [workstation-06]))
      (make-instance of Item (object-id [BEARING]) (location-id [workstation-07]))
      (make-instance of Item (object-id [BEARING_BOX]) (location-id [workstation-07]))
      (make-instance of Item (object-id [S40_40_G]) (location-id [workstation-06]))
      (make-instance of Item (object-id [AXIS]) (location-id [workstation-06]))
      (make-instance of Item (object-id [M30]) (location-id [workstation-06]))
    )

    ; Manipulation Task
    (slot-insert$ [task-info] tasks 5
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [MOTOR])
          (quantity-requested 1)
          (destination-id [workstation-06])
          (source-id [workstation-07])))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [BEARING])
          (quantity-requested 1)
          (destination-id [workstation-11])
          (source-id [workstation-07])))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [M20_100])
          (quantity-requested 1)
          (destination-id [workstation-07])
          (source-id [workstation-06])))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [R20])
          (quantity-requested 1)
          (destination-id [workstation-11])
          (source-id [workstation-06])))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [M30])
          (quantity-requested 1)
          (destination-id [workstation-11])
          (source-id [workstation-06])))
      )
    )
  ;)

)


;; BTT 2

(defclass BasicTransportationTest2 (is-a BasicTransportationTest) (role concrete))

(defmessage-handler BasicTransportationTest2 generate ()
  (printout t "Generating new BasicTransportationTest2" crlf)

  (bind ?robocup-objects          ?*ROBOCUP-OBJECTS*)
  (bind ?rockin-objects           ?*ROCKIN-OBJECTS*)
  (bind ?workstation-locations    ?*WORKSTATION-10CM-LOCATIONS*)
  (bind ?shelf-locations          ?*SHELF-LOCATIONS*)
  (bind ?rotating-table-locations ?*ROTATING-TABLE-LOCATIONS*)

  ; create empty list of objects for transportation
  (bind ?transportation-objects (create$ ))
  ; pick objects from the robocup set
  (loop-for-count 2
    (bind ?transportation-objects (create$ ?transportation-objects
      (pick-random$ ?robocup-objects)))
  )
  ; pick bjects from the RoCKIn set
  (loop-for-count 3
    (bind ?transportation-objects (create$ ?transportation-objects
      (pick-random$ ?rockin-objects)))
  )
  ; shuffle the objects
  (bind ?transportation-objects (randomize$ ?transportation-objects))

  ; Source locations must exist before adding to it.
  (bind ?source-locations (create$ ))
  ; Draw 2 source locations in a loop, this counter could be configured in the future.
  (loop-for-count (?counter 1 2) do
    (bind ?location  (pick-random$ ?workstation-locations))
    (bind ?source-locations (create$ ?source-locations ?location))
    (bind ?workstation-locations (delete-member$ ?workstation-locations ?location))
  )

  ; Create a list for desintation locations start with one shelf
  (bind ?location (pick-random$ ?shelf-locations))
  (bind ?destination-locations (create$ ?location))
  (loop-for-count 2 do
    (bind ?location (pick-random$ ?workstation-locations))
    (bind ?destination-locations (create$ ?destination-locations ?location))
    (bind ?workstation-locations (delete-member$ ?workstation-locations ?location))
  )
  ; shuffle the list of destination locations
  (bind ?destination-locations (randomize$ ?destination-locations))

  ; Need 5 transportation tasks
  ; but on the last we create 2... one for each container. and the rotating table is done later.
  (bind ?last 3)
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
    ; On the last iteration we add an extra item for the second container
    (if (neq ?counter ?last) then
      ; Task
      (slot-insert$ [task-info] tasks 1
        (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
          (transportation-task (make-instance of TransportationTask
            (object-id ?item)
            (quantity-requested 1)
            (destination-id ?destination-location)
            (source-id ?source-location))))
      )
    else
      ; 1st item for container
      (slot-insert$ [task-info] tasks 1
        (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
          (transportation-task (make-instance of TransportationTask
            (object-id ?item)
            (container-id [CONTAINER_R])
            (quantity-requested 1)
            (destination-id ?destination-location)
            (source-id ?source-location))))
      )
      ; increase the counter and get next item and source location. Can not bind to counter in side loop.
      (bind ?last (+ 1 ?last))
      (bind ?item (nth$ ?last ?transportation-objects))
      (bind ?source-location (nth$ (+ 1 (mod ?last (length ?source-locations))) ?source-locations))
      ; Inventory
      (slot-insert$ [inventory] items 1
        (make-instance of Item (object-id ?item)
                               (location-id ?source-location))
      )
      ; 2nd item for container
      (slot-insert$ [task-info] tasks 1
        (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
          (transportation-task (make-instance of TransportationTask
            (object-id ?item)
            (container-id [CONTAINER_B])
            (quantity-requested 1)
            (destination-id ?destination-location)
            (source-id ?source-location))))
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
)

;; BTT 3

(defclass BasicTransportationTest3 (is-a BasicTransportationTest) (role concrete))

(defmessage-handler BasicTransportationTest3 generate ()
  (printout t "Generating new BasicTransportationTest3" crlf)

  ; Get Globals
  (bind ?robocup-objects                ?*ROBOCUP-OBJECTS*)
  (bind ?rockin-objects                 ?*ROCKIN-OBJECTS*)
  (bind ?shelf-locations                ?*SHELF-LOCATIONS*)
  (bind ?rotating-table-locations       ?*ROTATING-TABLE-LOCATIONS*)
  (bind ?workstation-locations          ?*WORKSTATION-10CM-LOCATIONS*)
  (bind ?decoy-objects (create$         ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*))

  ; create empty list of objects for transportation
  (bind ?transportation-objects (create$ ))
  ; pick 4 objects from the robocup set
  (loop-for-count 3 do
    (bind ?item (pick-random$ ?robocup-objects))
    (bind ?transportation-objects (create$ ?transportation-objects ?item))
    (bind ?decoy-objects (delete-member$ ?decoy-objects ?item))
  )
  ; pick 3 objects from the RoCKIn set
  (loop-for-count 3 do
    (bind ?item (pick-random$ ?rockin-objects))
    (bind ?transportation-objects (create$ ?transportation-objects ?item))
    (bind ?decoy-objects (delete-member$ ?decoy-objects ?item))
  )
  ; shuffle the objects
  (bind ?transportation-objects (randomize$ ?transportation-objects))

    (printout t "Got 6 Objects" crlf)
  ; Source Locations
  (bind ?source-locations (create$ [workstation-06] [workstation-07]))
  


  ; Destination Locations
  ; first shelf
  (bind ?location (pick-random$ ?shelf-locations))
  (bind ?destination-locations ?location )
  (bind ?shelf-locations (delete-member$ ?shelf-locations ?location))
  
  
  ; Add other shelf as source
  ; Shuffle list of source locations
  (bind ?source-locations (create$ ?source-locations ?shelf-locations))
  (bind ?source-locations (randomize$ ?source-locations))
  
    (printout t "Got 2 Sources and a Shelf" crlf)
  ; then add all 10CM workstations
  
  (bind ?destination-locations (create$ ?destination-locations ?workstation-locations )) 
  
  ; Shuffle list of source locations
  (bind ?destination-locations (randomize$ ?destination-locations))

    (printout t "Got 3 Destinations" crlf)
  ; Need  transportation tasks Container 1

    (bind ?source-location (nth$ 1 ?source-locations))
    (bind ?destination-location (nth$ 1 ?destination-locations))
    (bind ?item (nth$ 1 ?transportation-objects))
    ; Inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item)
                             (location-id ?source-location))
    )
    
	; 1st item for container
	(slot-insert$ [task-info] tasks 1
	  (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
	    (transportation-task (make-instance of TransportationTask
	    (object-id ?item)
	    (container-id [CONTAINER_R])
	    (quantity-requested 1)
	    (destination-id ?destination-location)
	    (source-id ?source-location)))))
	(bind ?item (nth$ 2 ?transportation-objects))
	(bind ?source-location (nth$ 2 ?source-locations))
    	; Inventory
    	(slot-insert$ [inventory] items 1
      	(make-instance of Item (object-id ?item)
                             (location-id ?source-location))
   	 )
   	 
	; 2nd item for container
	(slot-insert$ [task-info] tasks 1
	  (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
	    (transportation-task (make-instance of TransportationTask
	      (object-id ?item)
	      (container-id [CONTAINER_R])
	      (quantity-requested 1)
	      (destination-id ?destination-location)
	      (source-id ?source-location)))))
	   
    (printout t "Got Red container" crlf)	   
  ; Need  transportation tasks

    (bind ?source-location (nth$ 2 ?source-locations))
    (bind ?destination-location (nth$ 2 ?destination-locations))
    (bind ?item (nth$ 3 ?transportation-objects))

    ; Inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item)
                             (location-id ?source-location))
    )

	; 1st item for container
	(slot-insert$ [task-info] tasks 1
	  (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
	    (transportation-task (make-instance of TransportationTask
	    (object-id ?item)
	    (container-id [CONTAINER_B])
	    (quantity-requested 1)
	    (destination-id ?destination-location)
	    (source-id ?source-location)))))

	; 2nd item for container
	(bind ?source-location (nth$ 3 ?source-locations))
	(bind ?item (nth$ 4 ?transportation-objects))
    	; Inventory
    	(slot-insert$ [inventory] items 1
      	(make-instance of Item (object-id ?item)
                             (location-id ?source-location))
    	)
	(slot-insert$ [task-info] tasks 1
	  (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
	    (transportation-task (make-instance of TransportationTask
	      (object-id ?item)
	      (container-id [CONTAINER_B])
	      (quantity-requested 1)
	      (destination-id ?destination-location)
	      (source-id ?source-location)))))

      (printout t "Got Blue container" crlf)

  ; One object must be placed on the rotating table
  (bind ?item (nth$ 5 ?transportation-objects))
  (bind ?source-location (nth$ 3 ?source-locations))
  (bind ?destination-location (pick-random$ ?workstation-locations))
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
      (printout t "Got task to normal table" crlf)
    ; One object must be placed on the rotating table
  (bind ?item (nth$ 6 ?transportation-objects))
  (bind ?source-location (nth$ 1 ?source-locations))
  (bind ?destination-location (nth$ 3 ?destination-locations))
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
  
      (printout t "Got Task to shelf" crlf)
  (bind ?counter 1)
  (loop-for-count (?counter 1 3) do
    ; Get a source location from source locations.
    ; Try to evenly distribute source locations.
    (bind ?source-location (nth$ (+ 1 (mod ?counter (length ?source-locations))) ?source-locations))
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id (pick-random$ ?decoy-objects)) (location-id ?source-location))
    )
  )
      (printout t "Got Decoys" crlf)
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
