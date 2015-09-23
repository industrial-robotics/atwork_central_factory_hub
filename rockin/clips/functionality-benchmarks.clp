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


(deffunction functionality-benchmarks-fbm1-init (?time)
  (make-instance [stopped-state] of FbmStoppedState (phase EXECUTION) (time ?time))
  (make-instance [running-state] of FbmRunningState (phase EXECUTION) (time ?time) (max-time ?*FBM1-TIME*))
  (make-instance [paused-state] of PausedState (phase EXECUTION))
  (make-instance [check-runs-state] of CheckRunsState (phase EXECUTION) (time ?time) (max-runs ?*FBM1-COUNT*))
  (make-instance [finished-state] of FinishedState (phase EXECUTION))

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [check-runs-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [check-runs-state])
  (send [running-state]    add-transition FINISH          [check-runs-state])
  (send [paused-state]     add-transition START           [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])
  (send [check-runs-state] add-transition REPEAT          [stopped-state])
  (send [check-runs-state] add-transition FINISH          [finished-state])


  (make-instance [sm] of StateMachine
    (current-state [stopped-state])
    (states [stopped-state] [running-state] [paused-state] [check-runs-state] [finished-state])
  )
)

(deffunction functionality-benchmarks-fbm2-init (?time)
  (make-instance [stopped-state] of FbmStoppedState (phase EXECUTION) (time ?time))
  (make-instance [running-state] of FbmRunningState (phase EXECUTION) (time ?time) (max-time ?*FBM2-TIME*))
  (make-instance [paused-state] of PausedState (phase EXECUTION))
  (make-instance [check-runs-state] of CheckRunsState (phase EXECUTION) (time ?time) (max-runs ?*FBM2-COUNT*))
  (make-instance [finished-state] of FinishedState (phase EXECUTION))

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [check-runs-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [check-runs-state])
  (send [running-state]    add-transition FINISH          [check-runs-state])
  (send [paused-state]     add-transition START           [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])
  (send [check-runs-state] add-transition REPEAT          [stopped-state])
  (send [check-runs-state] add-transition FINISH          [finished-state])

  (make-instance [sm] of StateMachine
    (current-state [stopped-state])
    (states [stopped-state] [running-state] [paused-state] [check-runs-state] [finished-state])
  )
)
