;---------------------------------------------------------------------------
;  functionality-benchmarks.clp - RoCKIn RefBox CLIPS functionality benchmarks
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deffunction functionality-benchmarks-fbm1-init ()
  (make-instance [init-state] of InitState)
  (make-instance [stopped-state] of StoppedState)
  (make-instance [running-state] of RunningState)
  (make-instance [paused-state] of PausedState)
  (make-instance [check-runs-state] of CheckRunsState (max-runs *FBM1-COUNT*))
  (make-instance [finished-state] of FinishedState)

  (send [init-state]       add-transition VALID_BENCHMARK [stopped-state])
  (send [stopped-state]    add-transition RESET           [init-state])
  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition RESET           [init-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [check-runs-state])
  (send [running-state]    add-transition FINISH          [check-runs-state])
  (send [paused-state]     add-transition RESET           [init-state])
  (send [paused-state]     add-transition CONTINUE        [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])
  (send [check-runs-state] add-transition REPEAT          [stopped-state])
  (send [check-runs-state] add-transition FINISH          [finished-state])
  (send [finished-state]   add-transition RESET           [init-state])

  (make-instance [sm] of StateMachine
    (current-state [init-state])
    (states [init-state] [stopped-state] [running-state] [paused-state] [check-runs-state] [finished-state])
  )
)

(deffunction functionality-benchmarks-fbm2-init ()
  (make-instance [init-state] of InitState)
  (make-instance [stopped-state] of StoppedState)
  (make-instance [running-state] of RunningState)
  (make-instance [paused-state] of PausedState)
  (make-instance [check-runs-state] of CheckRunsState (max-runs *FBM2-COUNT*))
  (make-instance [finished-state] of FinishedState)

  (send [init-state]       add-transition VALID_BENCHMARK [stopped-state])
  (send [stopped-state]    add-transition RESET           [init-state])
  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition RESET           [init-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [check-runs-state])
  (send [running-state]    add-transition FINISH          [check-runs-state])
  (send [paused-state]     add-transition RESET           [init-state])
  (send [paused-state]     add-transition CONTINUE        [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])
  (send [check-runs-state] add-transition REPEAT          [stopped-state])
  (send [check-runs-state] add-transition FINISH          [finished-state])
  (send [finished-state]   add-transition RESET           [init-state])

  (make-instance [sm] of StateMachine
    (current-state [init-state])
    (states [init-state] [stopped-state] [running-state] [paused-state] [check-runs-state] [finished-state])
  )
)

