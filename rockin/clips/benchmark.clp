;---------------------------------------------------------------------------
;  benchmark.clp - LLSF RefBox CLIPS benchmark maintenance
;
;  Created: Tue Jun 11 15:19:25 2013
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defrule benchmark-update-benchmark-time
  (declare (salience ?*PRIORITY_FIRST*))
  (time $?now)
  (benchmark-phase (id ?phase) (type ~NONE))
  ?gf <- (benchmark-state (phase-id ?phase) (state RUNNING) (benchmark-time ?benchmark-time) (last-time $?last-time&:(neq ?last-time ?now)))
  =>
  (bind ?timediff (time-diff-sec ?now ?last-time))
  (modify ?gf (benchmark-time (+ ?benchmark-time ?timediff)) (last-time ?now))
)

(defrule benchmark-update-last-time
  (declare (salience ?*PRIORITY_FIRST*))
  (time $?now)
  (or (and (benchmark-state (phase-id ?phase)) (benchmark-phase (id ?phase) (type NONE)))
      (benchmark-state (state ~RUNNING)))
  ?gf <- (benchmark-state (phase-id ?phase) (last-time $?last-time&:(neq ?last-time ?now)))
  =>
  (modify ?gf (last-time ?now))
)



(deffunction select-random-object ()
  (bind ?objects (create$))
  (do-for-all-facts ((?o object-identifier)) (eq ?o:type AX)
    (bind ?objects (insert$ ?objects 1 ?o))
  )

  (bind ?selected-object (pick-random$ ?objects))
  (bind ?description (nth$ 1 (fact-slot-value ?selected-object description)))

  (printout t "Place object " ?description " in front of the robot and continue the benchmark" crlf)
  (assert (attention-message (text (str-cat "The robot should handle the object " ?description))))

  ; Make the selected object available e.g. for logging
  (assert (selected-object (object-id (fact-slot-value ?selected-object id))))
)



(defrule benchmark-fbm-init
  (benchmark-phase (id ?phase) (type FBM) (type-id ?fbm-id))
  ?bs <- (benchmark-state (phase-id ?phase) (state INIT))
  (not (benchmark-initialized))
  =>
  (assert (benchmark-initialized))

  (switch ?fbm-id
    (case 1 then
      (modify ?bs (max-runs ?*FBM1-COUNT*) (max-time ?*FBM1-TIME*) (run 0))
    )
    (case 2 then
      (modify ?bs (max-runs ?*FBM2-COUNT*) (max-time ?*FBM2-TIME*) (run 0))
    )
  )

  (select-random-object)
)

(defrule benchmark-fbm-start-or-continue
  (benchmark-phase (id ?phase) (type FBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING) (prev-state INIT|PAUSED))
  ?so <- (selected-object)
  =>
  (retract ?so)
  (modify ?bs (prev-state RUNNING) (start-time (now)) (benchmark-time 0.0))

  (printout t "FBM: Start" crlf)
  (assert (attention-message (text "FBM: Start") (time 15)))
)

(defrule benchmark-fbm-run-timeout
  (benchmark-phase (id ?phase) (type FBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING)
           (max-runs ?max-runs) (run ?run&:(< ?run ?max-runs))
           (max-time ?max-time) (benchmark-time ?benchmark-time&:(>= ?benchmark-time ?max-time)))
  =>
  (modify ?bs (state PAUSED) (end-time (now)) (run (+ ?run 1)) (benchmark-time 0.0))

  (printout t "FBM: Run over" crlf)
  (assert (attention-message (text "FBM: Run over") (time 15)))

  (select-random-object)
)

(defrule benchmark-fbm-over
  (benchmark-phase (id ?phase) (type FBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING)
           (max-runs ?max-runs) (run ?run&:(>= ?run ?max-runs))
           (max-time ?max-time) (benchmark-time ?benchmark-time&:(>= ?benchmark-time ?max-time)))
  ?bf <- (benchmark-initialized)
  =>
  (retract ?bf)
  (modify ?bs (state FINISHED) (end-time (now)))

  (printout t "FBM: Benchmark over" crlf)
  (assert (attention-message (text "FBM: Benchmark over") (time 15)))
)
