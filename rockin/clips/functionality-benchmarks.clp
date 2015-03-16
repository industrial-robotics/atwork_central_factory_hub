;---------------------------------------------------------------------------
;  functionality-benchmarks.clp - RoCKIn RefBox CLIPS functionality benchmarks
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deffunction select-random-object ()
  (bind ?objects (create$ [ax-01] [ax-02] [ax-03] [ax-09] [ax-16] [em-01] [em-02]))

  (bind ?selected-object (pick-random$ ?objects))

  (bind ?description (nth$ 1 (send ?selected-object get-description)))
  (printout t "Place object " ?description " in front of the robot and start the benchmark" crlf)
  (assert (attention-message (text (str-cat "The robot should handle the object " ?description))))
)




(defclass FbmStoppedState (is-a StoppedState))

(defmessage-handler FbmStoppedState on-enter (?prev-state)
  (select-random-object)
)

(deffunction functionality-benchmarks-fbm1-init ()
  (make-instance [stopped-state] of FbmStoppedState)
  (make-instance [running-state] of RunningState (max-time ?*FBM1-TIME*))
  (make-instance [paused-state] of PausedState)
  (make-instance [check-runs-state] of CheckRunsState (max-runs ?*FBM1-COUNT*))
  (make-instance [finished-state] of FinishedState)

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [stopped-state])
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

(deffunction functionality-benchmarks-fbm2-init ()
  (make-instance [stopped-state] of FbmStoppedState)
  (make-instance [running-state] of RunningState (max-time ?*FBM2-TIME*))
  (make-instance [paused-state] of PausedState)
  (make-instance [check-runs-state] of CheckRunsState (max-runs ?*FBM2-COUNT*))
  (make-instance [finished-state] of FinishedState)

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [stopped-state])
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
