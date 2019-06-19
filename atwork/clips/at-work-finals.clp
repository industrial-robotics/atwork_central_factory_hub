;---------------------------------------------------------------------------
;  at-work-finals.clp - AtWork RefBox CLIPS - RoboCup@Work Finals
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass AtWorkFinal (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler AtWorkFinal setup (?time ?state-machine)
  (make-instance [prep-timeup-state] of TimeoutState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*AWF-PREPARATION-TIME*))
  (make-instance [prep-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [exec-stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [exec-running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*AWF-EXECUTION-TIME*))
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

(defmessage-handler AtWorkFinal handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

;; AWF 1

(defclass AtWorkFinal2016 (is-a AtWorkFinal) (role concrete))

(defmessage-handler AtWorkFinal2016 generate ()
  (printout t "Generating new AtWorkFinal2016" crlf)

  (bind ?manipulation-robocup-objects ?*ROBOCUP-OBJECTS*)
  (bind ?manipulation-rockin-objects ?*ROCKIN-OBJECTS*)
  (bind ?manipulation-objects (create$
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-robocup-objects)
    (pick-random$ ?manipulation-rockin-objects)
    (pick-random$ ?manipulation-rockin-objects)
    (pick-random$ ?manipulation-rockin-objects)
    (pick-random$ ?manipulation-rockin-objects)
  ))

  ; We need to ensure one object is known to be from the RoboCup@Work
  ;  objects for the Precision Placement
  (bind ?item-1  (pick-random$ ?manipulation-robocup-objects))
  ; The remaining objects can be picked from the shuffled manipulation object set
  (bind ?manipulation-objects (randomize$ ?manipulation-objects))
  (bind ?item-2  (nth$ 1 ?manipulation-objects))
  (bind ?item-3  (nth$ 2 ?manipulation-objects))
  (bind ?item-4  (nth$ 3 ?manipulation-objects))
  (bind ?item-5  (nth$ 4 ?manipulation-objects))
  (bind ?item-6  (nth$ 5 ?manipulation-objects))
  (bind ?item-7  (nth$ 6 ?manipulation-objects))
  (bind ?item-8  (nth$ 7 ?manipulation-objects))
  (bind ?item-9  (nth$ 8 ?manipulation-objects))
  (bind ?item-10 (nth$ 9 ?manipulation-objects))

  (bind ?decoy-objects (create$ ?*ROBOCUP-OBJECTS* ?*ROCKIN-OBJECTS*))

  (bind ?workstation-locations (create$
    ?*WORKSTATION-0CM-LOCATIONS*
    ?*WORKSTATION-5CM-LOCATIONS*
    ?*WORKSTATION-10CM-LOCATIONS*
    ?*WORKSTATION-15CM-LOCATIONS*
  ))
  (bind ?workstation-0cm-locations  ?*WORKSTATION-0CM-LOCATIONS*)
  (bind ?workstation-5cm-locations  ?*WORKSTATION-5CM-LOCATIONS*)
  (bind ?workstation-10cm-locations ?*WORKSTATION-10CM-LOCATIONS*)
  (bind ?workstation-15cm-locations ?*WORKSTATION-15CM-LOCATIONS*)
  (bind ?shelf-locations            ?*SHELF-LOCATIONS*)
  (bind ?precision-locations        ?*PRECISION-LOCATIONS*)
  (bind ?rotating-table-locations   ?*ROTATING-TABLE-LOCATIONS*)

  ; Shuffle list of shelf locations
  (bind ?shelf-locations (randomize$ ?shelf-locations))


  ;;
  ; Ensure each of the required source locations is selected
  ;; 
  ; One source location is a rotating table
  (bind ?source-rotating (pick-random$ ?rotating-table-locations))

  ; One source location is a shelf
  (bind ?source-shelf (nth$ 1 ?shelf-locations))


  ; One source location from 0 cm workstation 
  (bind ?source-location-1 (pick-random$ ?workstation-0cm-locations))
  (bind ?workstation-0cm-locations (delete-member$ ?workstation-0cm-locations ?source-location-1))

  ; One source location from 5 cm workstation
  (bind ?source-location-2 (pick-random$ ?workstation-5cm-locations))
  (bind ?workstation-5cm-locations (delete-member$ ?workstation-5cm-locations ?source-location-2))

  ; One source location from 10 cm workstation
  (bind ?source-location-3 (pick-random$ ?workstation-10cm-locations))
  (bind ?workstation-10cm-locations (delete-member$ ?workstation-10cm-locations ?source-location-3))

  ; One source location from 15 cm workstation
  (bind ?source-location-4 (pick-random$ ?workstation-15cm-locations))
  (bind ?workstation-15cm-locations (delete-member$ ?workstation-15cm-locations ?source-location-4))


  ; Create list of workstation locations
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

  ; The 2018 Finals have 1 object on the Rotating Table, and 2 on a Shelf.

  ; Draw destination locations according to task instances
  ; 1. One precision placement
  (bind ?destination-ppt (pick-random$  ?precision-locations))

  ; 2. One Shelf
  (bind ?destination-shelf (nth$ 2 ?shelf-locations))

  ; 3. One destination location from 0 cm workstation 
  (bind ?destination-location-1 (pick-random$  ?workstation-0cm-locations))

  ; 4. One destination location from 5 cm workstation 
  (bind ?destination-location-2 (pick-random$  ?workstation-5cm-locations))

  ; 5. One destination location from 10 cm workstation 
  (bind ?destination-location-3 (pick-random$  ?workstation-10cm-locations))

  ; 6. One destination location from 15 cm workstation 
  (bind ?destination-location-4 (pick-random$  ?workstation-15cm-locations))

  ; Create list of destination locations from drawn destination locations
  (bind ?destination-locations (create$
    ?destination-location-1
    ?destination-location-2
    ?destination-location-3
    ?destination-location-4

  ))

  ; Shuffle list of destination locations
  (bind ?destination-locations (randomize$ ?destination-locations))
  ; Take new locations from shuffled locations
  (bind ?destination-location-1 (nth$ 1 ?destination-locations))
  (bind ?destination-location-2 (nth$ 2 ?destination-locations))
  (bind ?destination-location-3 (nth$ 3 ?destination-locations))
  (bind ?destination-location-4 (nth$ 4 ?destination-locations))


  ; Remove items from decoy set
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-1))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-2))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-3))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-4))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-5))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-6))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-7))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-8))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-9))
  (bind ?decoy-objects (delete-member$ ?decoy-objects ?item-10))
  ; Pick 5 random decoy objects
  (bind ?decoy-1 (pick-random$ ?decoy-objects))
  (bind ?decoy-2 (pick-random$ ?decoy-objects))
  (bind ?decoy-3 (pick-random$ ?decoy-objects))
  (bind ?decoy-4 (pick-random$ ?decoy-objects))
  (bind ?decoy-5 (pick-random$ ?decoy-objects))


  (printout t  " Decoys: "(create$ ?decoy-1 ?decoy-2 ?decoy-3 ?decoy-4 ?decoy-5) crlf)

  ; Inventory
  (slot-insert$ [inventory] items 1

    ; source location 1-2 Shelf
    (make-instance of Item (object-id ?item-1) (location-id ?source-shelf))
    (make-instance of Item (object-id ?item-2) (location-id ?source-shelf))
    ; source location 3 Rotating table
    (make-instance of Item (object-id ?item-3) (location-id ?source-rotating))
    ; source location 4 - 10
    (make-instance of Item (object-id ?item-4) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-5) (location-id ?source-location-2))
    (make-instance of Item (object-id ?item-6) (location-id ?source-location-3))
    (make-instance of Item (object-id ?item-7) (location-id ?source-location-4))
    (make-instance of Item (object-id ?item-8) (location-id ?source-location-1))
    (make-instance of Item (object-id ?item-9) (location-id ?source-location-2))
    (make-instance of Item (object-id ?item-10) (location-id ?source-location-3))

    ; One decoy for each source location
    (make-instance of Item (object-id ?decoy-1) (location-id ?source-location-1))
    (make-instance of Item (object-id ?decoy-2) (location-id ?source-location-1))
    (make-instance of Item (object-id ?decoy-3) (location-id ?source-location-2))
    (make-instance of Item (object-id ?decoy-4) (location-id ?source-location-2))
    (make-instance of Item (object-id ?decoy-5) (location-id ?source-rotating))

    ; Containers for two Tables
    (make-instance of Item (object-id [CONTAINER_B]) (location-id ?destination-location-1))
    (make-instance of Item (object-id [CONTAINER_R]) (location-id ?destination-location-1))
    (make-instance of Item (object-id [CONTAINER_B]) (location-id ?destination-location-2))
    (make-instance of Item (object-id [CONTAINER_R]) (location-id ?destination-location-2))
  )
  (printout t "Inventory defined" clrf)
  ; Tasks 

  (slot-insert$ [task-info] tasks 1

    ; Two Picks from shelf
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-9)
        (quantity-requested 1)
        (destination-id ?destination-location-2)
        (source-id ?source-shelf)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-2)
        (quantity-requested 1)
        (destination-id ?destination-location-4)
        (source-id ?source-shelf)
    )))

    ; One Pick from TurnTable
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-3)
        (quantity-requested 1)
        (destination-id ?destination-location-3)
        (source-id ?source-rotating)
    )))


    ; Four picks into containers
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-4)
        (quantity-requested 1)
        (container-id [CONTAINER_B])
        (destination-id ?destination-location-1)
        (source-id ?source-location-1)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-5)
        (quantity-requested 1)
        (container-id [CONTAINER_B])
        (destination-id ?destination-location-1)
        (source-id ?source-location-2)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-6)
        (quantity-requested 1)
        (container-id [CONTAINER_R])
        (destination-id ?destination-location-2)
        (source-id ?source-location-3)
    )))
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-7)
        (quantity-requested 1)
        (container-id [CONTAINER_R])
        (destination-id ?destination-location-2)
        (source-id ?source-location-4)
    )))


    ; One Place into Shelf
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-8)
        (quantity-requested 1)
        (destination-id ?destination-shelf)
        (source-id ?source-location-1)
    )))

    ; One PPT
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-1)
        (quantity-requested 1)
        (destination-id ?destination-ppt)
        (source-id ?source-location-2)
    )))
    ;Last one Normal
    (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
      (transportation-task (make-instance of TransportationTask
        (object-id ?item-10)
        (quantity-requested 1)
        (destination-id ?destination-location-4)
        (source-id ?source-location-3)
    )))
  )

  (printout t "Task defined" clrf)

)

;; END AWF1

(defrule init-awf
  (init)
  ?awf <- (object (is-a Benchmark))
  =>
  (make-instance [AWF1] of AtWorkFinal2016 (type AWF) (type-id 1) (description "At Work Final 2016"))

  (slot-insert$ ?awf registered-scenarios 1 [AWF1])
)
