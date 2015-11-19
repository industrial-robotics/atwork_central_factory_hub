;---------------------------------------------------------------------------
;  functionality-benchmarks.clp - RoCKIn RefBox CLIPS functionality benchmarks
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass FbmStoppedState (is-a StoppedState)
  (slot selected-object (type STRING))
)

(defmessage-handler FbmStoppedState on-enter (?prev-state)
  ; Select a random object to be handled by the robot
  (bind ?objects (create$ [ax-01] [ax-02] [ax-03] [ax-09] [ax-16] [em-01] [em-02]))

  (bind ?selected-object (pick-random$ ?objects))

  (bind ?description (nth$ 1 (send ?selected-object get-description)))
  (printout t "Place object " ?description " in front of the robot and start the benchmark" crlf)
  (assert (attention-message (text (str-cat "The robot should handle the object " ?description))))
  (bind ?self:selected-object ?description)


  ; Store the selected object so that it can be streamed to the clients
  ; continuously during the benchmark execution
  (assert (benchmark-info (object ?self:selected-object)))


  ; Call the parent function
  (call-next-handler)
)

(defmessage-handler FbmStoppedState on-exit (?next-state)
  ; Call the parent function
  (call-next-handler)


  ; Clean up the selected object
  ; Note: the running state should get the selected from the selected-object
  ; slot and keep publishing it
  (do-for-all-facts ((?info benchmark-info)) TRUE
    (retract ?info)
  )
)



(defclass FbmRunningState (is-a RunningState)
  (slot selected-object (type STRING))
)

(defmessage-handler FbmRunningState on-enter (?prev-state)
  ; Call the parent function
  (call-next-handler)

  ; If we enter from an FbmStoppedState, remember the selected object
  (do-for-instance ((?s FbmStoppedState)) (eq ?s ?prev-state)
    (bind ?self:selected-object (send ?prev-state get-selected-object))
  )

  ; Store the selected object so that it can be streamed to the clients
  ; continuously during the benchmark execution
  (assert (benchmark-info (object ?self:selected-object)))
)

(defmessage-handler FbmRunningState on-exit (?next-state)
  ; Clean up the selected object
  (do-for-all-facts ((?info benchmark-info)) TRUE
    (retract ?info)
  )

  ; Call the parent function
  (call-next-handler)
)




(defclass FunctionalityBenchmark1 (is-a BenchmarkScenario) (role concrete))
(defclass FunctionalityBenchmark2 (is-a BenchmarkScenario) (role concrete))
(defclass FunctionalityBenchmark3 (is-a BenchmarkScenario) (role concrete))

(defmessage-handler FunctionalityBenchmark1 setup (?time ?state-machine)
  (make-instance [stopped-state] of FbmStoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of FbmRunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*FBM1-TIME*))
  (make-instance [paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [check-runs-state] of CheckRunsState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-runs ?*FBM1-COUNT*))
  (make-instance [finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [check-runs-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [check-runs-state])
  (send [running-state]    add-transition FINISH          [check-runs-state])
  (send [paused-state]     add-transition START           [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])
  (send [check-runs-state] add-transition REPEAT          [stopped-state])
  (send [check-runs-state] add-transition FINISH          [finished-state])


  (make-instance ?state-machine of StateMachine
    (current-state [stopped-state])
    (states [stopped-state] [running-state] [paused-state] [check-runs-state] [finished-state])
  )
)

(defmessage-handler FunctionalityBenchmark1 handle-feedback (?pb-msg ?time ?name ?team)
  (if (and
       (pb-has-field ?pb-msg "object_class_name")
       (pb-has-field ?pb-msg "object_instance_name")
       (pb-has-field ?pb-msg "object_pose"))
   then
    (printout t "FBM: Robot " ?name "/" ?team
        " recognized object instance " (pb-field-value ?pb-msg "object_instance_name")
        " of class " (pb-field-value ?pb-msg "object_class_name") crlf)
    (assert (attention-message (text (str-cat "FBM: Robot " ?name "/" ?team
        " recognized object instance" (pb-field-value ?pb-msg "object_instance_name")
        " of class " (pb-field-value ?pb-msg "object_class_name")))))
   else
    (printout t "Benchmark feedback from " ?name "/" ?team " is invalid" crlf)
  )

  (return FINISH)
)




(defmessage-handler FunctionalityBenchmark2 setup (?time ?state-machine)
  (make-instance [preparation-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [preparation-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*FBM2-PREPARATION-TIME*))
  (make-instance [preparation-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [execution-stopped-state] of FbmStoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [execution-running-state] of FbmRunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*FBM2-EXECUTION-TIME*))
  (make-instance [execution-paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [execution-check-runs-state] of CheckRunsState
    (phase EXECUTION) (state-machine ?state-machine)(time ?time) (max-runs ?*FBM2-COUNT*))
  (make-instance [execution-finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [preparation-stopped-state]  add-transition START   [preparation-running-state])
  (send [preparation-running-state]  add-transition PAUSE   [preparation-paused-state])
  (send [preparation-running-state]  add-transition STOP    [execution-stopped-state])
  (send [preparation-running-state]  add-transition TIMEOUT [execution-stopped-state])
  (send [preparation-running-state]  add-transition FINISH  [execution-stopped-state])
  (send [preparation-paused-state]   add-transition START   [preparation-running-state])
  (send [preparation-paused-state]   add-transition STOP    [execution-stopped-state])

  (send [execution-stopped-state]    add-transition START   [execution-running-state])
  (send [execution-running-state]    add-transition STOP    [execution-check-runs-state])
  (send [execution-running-state]    add-transition PAUSE   [execution-paused-state])
  (send [execution-running-state]    add-transition TIMEOUT [execution-check-runs-state])
  (send [execution-running-state]    add-transition FINISH  [execution-check-runs-state])
  (send [execution-paused-state]     add-transition START   [execution-running-state])
  (send [execution-paused-state]     add-transition STOP    [execution-stopped-state])
  (send [execution-check-runs-state] add-transition REPEAT  [preparation-running-state])
  (send [execution-check-runs-state] add-transition FINISH  [execution-finished-state])

  (make-instance ?state-machine of StateMachine
    (current-state [preparation-stopped-state])
    (states
      [preparation-stopped-state] [preparation-running-state]
      [preparation-paused-state] [preparation-finished-state]
      [execution-stopped-state] [execution-running-state]
      [execution-paused-state] [execution-check-runs-state]
      [execution-finished-state]
    )
  )
)

(defmessage-handler FunctionalityBenchmark2 handle-feedback (?pb-msg ?time ?name ?team)
  (if (and
       (pb-has-field ?pb-msg "grasp_notification")
       (pb-has-field ?pb-msg "object_class_name")
       (pb-has-field ?pb-msg "object_instance_name")
       (pb-has-field ?pb-msg "end_effector_pose"))
   then
    (printout t "FBM: Robot " ?name "/" ?team
        " lifted object instance " (pb-field-value ?pb-msg "object_instance_name")
        " of class " (pb-field-value ?pb-msg "object_class_name") crlf)
    (assert (attention-message (text (str-cat "FBM: Robot " ?name "/" ?team
        " lifted object instance " (pb-field-value ?pb-msg "object_instance_name")
        " of class " (pb-field-value ?pb-msg "object_class_name")))))
   else
    (printout t "Benchmark feedback from " ?name "/" ?team " is invalid" crlf)
  )

  (return FINISH)
)




(defmessage-handler FunctionalityBenchmark3 setup (?time ?state-machine)
  (make-instance [calibration-stopped-state] of StoppedState
    (phase CALIBRATION) (state-machine ?state-machine) (time ?time))
  (make-instance [calibration-running-state] of RunningState
    (phase CALIBRATION) (state-machine ?state-machine) (time ?time) (max-time ?*FBM3-CALIBRATION-TIME*))
  (make-instance [calibration-paused-state] of PausedState
    (phase CALIBRATION) (state-machine ?state-machine))
  (make-instance [calibration-finished-state] of FinishedState
    (phase CALIBRATION) (state-machine ?state-machine))

  (make-instance [preparation-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [preparation-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*FBM3-PREPARATION-TIME*))
  (make-instance [preparation-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [execution-stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [execution-running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*FBM3-EXECUTION-TIME*))
  (make-instance [execution-paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [execution-check-runs-state] of CheckRunsState
    (phase EXECUTION) (state-machine ?state-machine)(time ?time) (max-runs ?*FBM3-COUNT*))
  (make-instance [execution-finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [calibration-stopped-state]  add-transition START   [calibration-running-state])
  (send [calibration-running-state]  add-transition PAUSE   [calibration-paused-state])
  (send [calibration-running-state]  add-transition STOP    [calibration-finished-state])
  (send [calibration-running-state]  add-transition TIMEOUT [calibration-finished-state])
  (send [calibration-running-state]  add-transition FINISH  [calibration-finished-state])
  (send [calibration-paused-state]   add-transition START   [calibration-running-state])
  (send [calibration-paused-state]   add-transition STOP    [calibration-finished-state])
  (send [calibration-finished-state] add-transition START   [preparation-stopped-state])

  (send [preparation-stopped-state]  add-transition START   [preparation-running-state])
  (send [preparation-running-state]  add-transition PAUSE   [preparation-paused-state])
  (send [preparation-running-state]  add-transition STOP    [execution-stopped-state])
  (send [preparation-running-state]  add-transition TIMEOUT [execution-stopped-state])
  (send [preparation-running-state]  add-transition FINISH  [execution-stopped-state])
  (send [preparation-paused-state]   add-transition START   [preparation-running-state])
  (send [preparation-paused-state]   add-transition STOP    [execution-stopped-state])

  (send [execution-stopped-state]    add-transition START   [execution-running-state])
  (send [execution-running-state]    add-transition STOP    [execution-check-runs-state])
  (send [execution-running-state]    add-transition PAUSE   [execution-paused-state])
  (send [execution-running-state]    add-transition TIMEOUT [execution-check-runs-state])
  (send [execution-running-state]    add-transition FINISH  [execution-check-runs-state])
  (send [execution-paused-state]     add-transition START   [execution-running-state])
  (send [execution-paused-state]     add-transition STOP    [execution-stopped-state])
  (send [execution-check-runs-state] add-transition REPEAT  [calibration-stopped-state])
  (send [execution-check-runs-state] add-transition FINISH  [execution-finished-state])

  (make-instance ?state-machine of StateMachine
    (current-state [calibration-stopped-state])
    (states
      [calibration-stopped-state] [calibration-running-state]
      [calibration-paused-state] [calibration-finished-state]
      [preparation-stopped-state] [preparation-running-state]
      [preparation-paused-state] [preparation-finished-state]
      [execution-stopped-state] [execution-running-state]
      [execution-paused-state] [execution-check-runs-state]
      [execution-finished-state]
    )
  )
)

(defmessage-handler FunctionalityBenchmark3 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)
)




(defrule init-fbm
  (init)
  ?bm <- (object (is-a Benchmark))
  =>
  (make-instance [FBM1] of FunctionalityBenchmark1 (type FBM) (type-id 1) (description "Object Perception Functionality"))
  (make-instance [FBM2] of FunctionalityBenchmark2 (type FBM) (type-id 2) (description "Manipulation Functionality"))
  (make-instance [FBM3] of FunctionalityBenchmark3 (type FBM) (type-id 3) (description "Control Functionality"))

  (slot-insert$ ?bm registered-scenarios 1 [FBM1])
  (slot-insert$ ?bm registered-scenarios 1 [FBM2])
  (slot-insert$ ?bm registered-scenarios 1 [FBM3])
)
