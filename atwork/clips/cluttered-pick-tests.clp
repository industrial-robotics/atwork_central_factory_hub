;---------------------------------------------------------------------------
;  basic-manipulation-tests.clp - AtWork RefBox CLIPS - BMTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicManipulationTest (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler BasicManipulationTest setup (?time ?state-machine)
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
)

(defmessage-handler BasicManipulationTest handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

;; BMT 1

(defclass BasicManipulationTest1 (is-a BasicManipulationTest) (role concrete))

(defmessage-handler BasicManipulationTest1 generate ()
  (printout t "Generating Replay Basic Manipulation Test: WM 2019 Sydney" crlf)

  ;(bind ?manipulation-robocup-objects ?*ROBOCUP-OBJECTS*)
  
  ;(bind ?manipulation-rockin-objects ?*ROCKIN-OBJECTS*)

  ; set static location for source
  (bind ?source-location [workstation-15])
  ; set static location for destination
  (bind ?destination-location [workstation-16])

  ; 3 RoboCup objects
  ;(loop-for-count 3
    ; Pick random RoboCup object
 ;   (bind ?item (pick-random$ ?manipulation-robocup-objects))

    ; Add to inventory
    (slot-insert$ [inventory] items 5
      (make-instance of Item (object-id [BEARING_BOX]) (location-id ?source-location))
      (make-instance of Item (object-id [DISTANCE_TUBE])    (location-id ?source-location))
      (make-instance of Item (object-id [M20_100])      (location-id ?source-location))
      (make-instance of Item (object-id [M20]) (location-id ?source-location))
      (make-instance of Item (object-id [S40_40_G]) (location-id ?source-location))
    )

    ; Manipulation Task
    (slot-insert$ [task-info] tasks 5
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [BEARING_BOX])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [DISTANCE_TUBE])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [M20_100])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [M20])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [S40_40_G])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
     )
  ;)
)


(defrule init-bmt
  (init)
  ?bmt <- (object (is-a Benchmark))
  =>
  (make-instance [BMT1] of BasicManipulationTest1 (type BMT) (type-id 1) (description "Basic Manipulation Test"))

  (slot-insert$ ?bmt registered-scenarios 1 [BMT1])
)
