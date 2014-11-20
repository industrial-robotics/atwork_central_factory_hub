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
