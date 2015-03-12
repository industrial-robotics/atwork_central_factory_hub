;---------------------------------------------------------------------------
;  state-machine.clp - RoCKIn RefBox CLIPS state machine
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass StateMachine (is-a USER) (role concrete)
  (slot current-state (type INSTANCE) (allowed-classes State))
  (slot previous-state (type INSTANCE) (allowed-classes State))
  (multislot states (type INSTANCE) (allowed-classes State))
)

(defclass Transition (is-a USER) (role concrete)
  (slot input (type SYMBOL))
  (slot next-state (type INSTANCE) (allowed-classes State))
)

(defmessage-handler StateMachine set-state (?state)
  ; TODO: Check that state is part of the state machine

  (send ?self:current-state on-exit ?state)
  (bind ?self:previous-state ?self:current-state)
  (bind ?self:current-state ?state)
  (send ?self:current-state on-enter ?self:previous-state)
)

(defmessage-handler StateMachine process-event (?event)
  (bind ?valid-transitions (send ?self:current-state get-transitions))

  ; iterate over all transitions of the current state
  ; and check if any of them can handle the event
  (foreach ?t ?valid-transitions
    (if (eq (send ?t get-input) ?event) then
      (bind ?next-state (send ?t get-next-state))
      (if (debug 2) then (printout t "Transition state from " ?self:current-state " to " ?next-state crlf))
      (send ?self set-state ?next-state)
      (return)
    )
  )
  
  (printout t "Ignoring event " ?event " in state " ?self:current-state crlf)
)

(defmessage-handler StateMachine update ()
  (send ?self:current-state on-update)
)

(defclass State (is-a USER) (role abstract)
  (multislot transitions (type INSTANCE) (allowed-classes Transition))
)

(defmessage-handler State on-enter (?prev-state))
(defmessage-handler State on-update ())
(defmessage-handler State on-exit (?next-state))
(defmessage-handler State to-robot-state ())
(defmessage-handler State add-transition (?input ?next-state)
  (bind ?t (make-instance of Transition (input ?input) (next-state ?next-state)))
  (slot-insert$ ?self transitions 1 ?t)
)


(defclass InitState (is-a State) (role concrete))
(defclass StoppedState (is-a State) (role concrete))
(defclass RunningState (is-a State) (role concrete))
(defclass PausedState (is-a State) (role concrete))
(defclass CheckRunsState (is-a State) (role concrete)
  (slot run (type INTEGER) (default 0))             ; how often the specific benchmark has been executed already
  (slot max-runs (type INTEGER) (default 1))
)
(defclass FinishedState (is-a State) (role concrete))


(defmessage-handler InitState on-enter (?prev-state)
  ; Remove all items from the inventory
  (foreach ?item (send [inventory] get-items)
    (unmake-instance ?item)
  )
  (slot-delete$ [inventory] items 1 (length$ (send [inventory] get-items)))

  ; Remove all orders from the order info
  (foreach ?order (send [order-info] get-orders)
    (unmake-instance ?order)
  )
  (slot-delete$ [order-info] orders 1 (length$ (send [order-info] get-orders)))
)

(defmessage-handler InitState to-robot-state ()
  (return INIT)
)


(defmessage-handler StoppedState on-exit (?next-state)
  (printout t "Starting benchmark " crlf)
  (assert (attention-message (text "Starting benchmark") (time 15)))

  ; reset the start time of the benchmark
  (bind ?now (now))
  (do-for-all-facts ((?bs benchmark-state)) TRUE
    (modify ?bs (start-time ?now))
  )
)

(defmessage-handler StoppedState to-robot-state ()
  (return PAUSED)
)


(defmessage-handler RunningState on-enter (?prev-state)
  ; reset the time of the last update when entering the running state
  (bind ?now (now))
  (do-for-all-facts ((?bs benchmark-state)) TRUE
    (modify ?bs (last-time ?now))
  )
)

(defmessage-handler RunningState on-update ()
  ; continuously update the benchmark time
  (bind ?now (now))
  (do-for-all-facts ((?bs benchmark-state)) TRUE
    (bind ?timediff (time-diff-sec ?now ?bs:last-time))
    (modify ?bs (benchmark-time (+ ?bs:benchmark-time ?timediff)) (last-time ?now))
  )
)

(defmessage-handler RunningState to-robot-state ()
  (return RUNNING)
)


(defmessage-handler PausedState on-enter (?prev-state)
  (printout t "Pausing benchmark " crlf)
  (assert (attention-message (text "Pausing benchmark") (time 15)))
)

(defmessage-handler PausedState on-exit (?next-state)
  (printout t "Continuing benchmark " crlf)
  (assert (attention-message (text "Continuing benchmark") (time 15)))
)

(defmessage-handler PausedState to-robot-state ()
  (return PAUSED)
)


(defmessage-handler CheckRunsState on-enter (?prev-state)
  (do-for-fact ((?bs benchmark-state)) TRUE
    (printout t "Run " ?self:run " over after " ?bs:time " seconds" crlf)
    (assert (attention-message (text (str-cat "Run " ?self:run " over after " ?bs:time " seconds")) (time 15)))

    (modify ?bs (end-time (now)) (run (+ ?self:run 1)) (benchmark-time 0.0))
  )
)

(defmessage-handler CheckRunsState on-update ()
  (do-for-all-facts ((?bs benchmark-state)) TRUE
    (if (>= ?self:run ?self:max-runs) then
      (printout t "Benchmark over" crlf)
      (assert (attention-message (text "Benchmark over") (time 15)))
      (send [sm] process-event FINISH)
    else
      (send [sm] process-event REPEAT)
    )
  )
)

(defmessage-handler CheckRunsState to-robot-state ()
  (return PAUSED)
)


(defmessage-handler FinishedState to-robot-state ()
  (return FINISHED)
)


; instantiate a dummy state machine
(defrule state-machine-init
  (init)
  =>
  (make-instance [init-state] of InitState)
  (make-instance [sm] of StateMachine
    (current-state [init-state])
    (states [init-state])
  )
)

(defrule state-machine-update
  (time $?now)
  (object (is-a StateMachine))
  =>
  (send [sm] update)
)

