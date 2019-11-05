;---------------------------------------------------------------------------
;  basic-manipulation-tests.clp - AtWork RefBox CLIPS - BMTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicTransportationTestShort (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler BasicTransportationTestShort setup (?time ?state-machine)
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
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BTT1-EXECUTION-TIME*))
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

(defmessage-handler BasicTransportationTestShort handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

(defclass BasicTransportationTest1 (is-a BasicTransportationTestShort) (role concrete))

(defmessage-handler BasicTransportationTest1 generate ()
  (printout t "Generating Replay Basic Transportation Test 1: WM 2019 Sydney" crlf)

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
      (make-instance of Item (object-id [MOTOR])     (location-id [workstation-03]))
      (make-instance of Item (object-id [M20])       (location-id [workstation-05]))
      (make-instance of Item (object-id [F20_20_G])  (location-id [workstation-05]))
      (make-instance of Item (object-id [F20_20_B]) (location-id [workstation-06]))
      (make-instance of Item (object-id [M20_100])   (location-id [workstation-06]))
    )

    ; Manipulation Task
    (slot-insert$ [task-info] tasks 5
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [MOTOR])
          (quantity-requested 1)
          (destination-id [workstation-16])
          (source-id [workstation-03])))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [M20])
          (quantity-requested 1)
          (destination-id [workstation-16])
          (source-id [workstation-05])))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [F20_20_G])
          (quantity-requested 1)
          (destination-id [workstation-15])
          (source-id [workstation-05])))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [F20_20_B])
          (quantity-requested 1)
          (destination-id [workstation-16])
          (source-id [workstation-06])))
      )
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id [M20_100])
          (quantity-requested 1)
          (destination-id [workstation-05])
          (source-id [workstation-06])))
      )
     )
  ;)
)

(defrule init-btt
  (init)
  ?btt <- (object (is-a Benchmark))
  =>
  (make-instance [BTT1] of BasicTransportationTest1 (type BTT) (type-id 1) (description "Basic Transportation Test 1"))

  (slot-insert$ ?btt registered-scenarios 1 [BTT1])
)
