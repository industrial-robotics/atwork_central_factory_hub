;---------------------------------------------------------------------------
;  basic-manipulation-tests.clp - AtWork RefBox CLIPS - BMTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass PrecisionPlacementTest (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler PrecisionPlacementTest setup (?time ?state-machine)
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
)

(defmessage-handler PrecisionPlacementTest handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

(defclass PrecisionPlacementTest1 (is-a PrecisionPlacementTest) (role concrete))

(defmessage-handler PrecisionPlacementTest1 generate ()
  (printout t "Generating Replay Precision Placement Test: WM 2019 Sydney" crlf)

  ;(bind ?manipulation-robocup-objects ?*ROBOCUP-OBJECTS*)
  
  ;(bind ?manipulation-rockin-objects ?*ROCKIN-OBJECTS*)

  ; set static location for source
  (bind ?source-location [workstation-13])
  ; set static location for destination
  (bind ?destination-location ?*PRECISION-LOCATIONS*)

  ; 3 RoboCup objects
  ;(loop-for-count 3
    ; Pick random RoboCup object
 ;   (bind ?item (pick-random$ ?manipulation-robocup-objects))

    ; Add to inventory
    (slot-insert$ [inventory] items 3
      (make-instance of Item (object-id [S40_40_B]) (location-id ?source-location))
      (make-instance of Item (object-id [R20])       (location-id ?source-location))
      (make-instance of Item (object-id [F20_20_G])   (location-id ?source-location))
    )

    ; Manipulation Task
    (slot-insert$ [task-info] tasks 3
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [S40_40_B])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [R20])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [F20_20_G])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
     )
  ;)
)


(defrule init-ppt
  (init)
  ?ppt <- (object (is-a Benchmark))
  =>
  (make-instance [PPT1] of PrecisionPlacementTest1 (type PPT) (type-id 1) (description "Precision Placement Test 1"))

  (slot-insert$ ?ppt registered-scenarios 1 [PPT1])
)
