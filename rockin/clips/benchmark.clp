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



(deffunction print-random-object ()
  (bind ?objects (create$))
  (do-for-all-facts ((?o object-identifier)) (eq ?o:type AX)
    (bind ?objects (insert$ ?objects 1 ?o))
  )

  (bind ?selected-object (pick-random$ ?objects))
  (bind ?description (nth$ 1 (fact-slot-value ?selected-object description)))

  (printout t "Place object " ?description " in front of the robot and continue the benchmark" crlf)
  (assert (attention-message (text (str-cat "The robot should handle the object " ?description))))
)



(defrule fbm1-init
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  ?bs <- (benchmark-state (phase-id ?phase) (state INIT))
  (not (fbm1-is-initialized))
  =>
  (assert (fbm1-is-initialized))

  (print-random-object)
)

(defrule fbm1-start
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING) (prev-state INIT))
  =>
  (modify ?bs (prev-state RUNNING) (start-time (now)) (run 1))

  (printout t "FBM1: Start" crlf)
  (assert (attention-message (text "FBM1: Start") (time 15)))
)

(defrule fbm1-run-timeout
  (time $?now)
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING)
           (run ?run&:(< ?run ?*FBM1-COUNT*))
           (benchmark-time ?benchmark-time&:(>= ?benchmark-time ?*FBM1-TIME*)))
  =>
  (modify ?bs (state PAUSED) (end-time (now)) (run (+ ?run 1)))

  (printout t "FBM1: Run over" crlf)
  (assert (attention-message (text "FBM1: Run over") (time 15)))

  (print-random-object)
)

(defrule fbm1-continue
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING) (prev-state PAUSED))
  =>
  (modify ?bs (prev-state RUNNING) (start-time (now)) (last-time (now)) (benchmark-time 0.0))

  (printout t "FBM1: Continue" crlf)
  (assert (attention-message (text "FBM1: Continue") (time 15)))
)

(defrule fbm1-over
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING)
           (run ?run&:(>= ?run ?*FBM1-COUNT*))
           (benchmark-time ?benchmark-time&:(>= ?benchmark-time ?*FBM1-TIME*)))
  =>
  (modify ?bs (state FINISHED) (end-time (now)))

  (printout t "FBM1: Benchmark over" crlf)
  (assert (attention-message (text "FBM1: Benchmark over") (time 15)))
)
