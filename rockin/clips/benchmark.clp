;---------------------------------------------------------------------------
;  benchmark.clp - LLSF RefBox CLIPS benchmark maintenance
;
;  Created: Tue Jun 11 15:19:25 2013
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deffunction benchmark-reset ()
  ; Retract all items
  (delayed-do-for-all-facts ((?i item)) TRUE
    (retract ?i)
  )

  ; Retract all orders
  (delayed-do-for-all-facts ((?o order)) TRUE
    (retract ?o)
  )
)

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
  (do-for-all-facts ((?o benchmark-object)) TRUE
    (bind ?objects (insert$ ?objects 1 ?o))
  )

  (bind ?selected-object (pick-random$ ?objects))
  (bind ?benchmark-id (fact-slot-value ?selected-object benchmark-id))
  (bind ?selected-object-id (fact-slot-value ?selected-object object-id))

  (do-for-fact ((?o object-identifier)) (eq ?o:id ?selected-object-id)
    (bind ?description (nth$ 1 (fact-slot-value ?o description)))

    (printout t "FBM: Place object " ?description " (" ?benchmark-id ") in front "
        "of the robot and continue the benchmark" crlf)
    (assert (attention-message (text (str-cat "FBM: The robot should handle the "
        "object " ?description " (" ?benchmark-id ")"))))

    ; Make the selected object available e.g. for logging
    (assert (selected-object (object-id ?o:id)))
  )
)


(defrule benchmark-switch-to-init
  ?bs <- (benchmark-state (state INIT) (prev-state ~INIT))
  =>
  (benchmark-reset)
)


; Initialize and directly transition to PAUSED state
(defrule benchmark-fbm-init
  (benchmark-phase (id ?phase) (type FBM) (type-id ?fbm-id))
  ?bs <- (benchmark-state (phase-id ?phase) (state INIT))
  =>
  (benchmark-reset)

  (switch ?fbm-id
    (case 1 then
      (modify ?bs (state PAUSED) (prev-state INIT) (max-runs ?*FBM1-COUNT*)
          (max-time ?*FBM1-TIME*) (run 1) (benchmark-time 0.0))
    )
    (case 2 then
      (modify ?bs (state PAUSED) (prev-state INIT) (max-runs ?*FBM2-COUNT*)
          (max-time ?*FBM2-TIME*) (run 1) (benchmark-time 0.0))
    )
  )
)

; Select an object when entering PAUSED state
(defrule benchmark-fbm1-switch-to-pause
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  ?bs <- (benchmark-state (phase-id ?phase) (state PAUSED) (prev-state ~PAUSED))
  =>
  (modify ?bs (prev-state PAUSED))

  (select-random-object)
)

; In FBM2 the RefBox does not select the object
(defrule benchmark-fbm2-switch-to-pause
  (benchmark-phase (id ?phase) (type FBM) (type-id 2))
  ?bs <- (benchmark-state (phase-id ?phase) (state PAUSED) (prev-state ~PAUSED))
  =>
  (modify ?bs (prev-state PAUSED))
)

; When a command switches the state from PAUSED to RUNNING, setup the current run
(defrule benchmark-fbm-run-start
  (benchmark-phase (id ?phase) (type FBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING) (prev-state PAUSED))
  ?so <- (selected-object)
  =>
  (retract ?so)
  (modify ?bs (prev-state RUNNING) (start-time (now)) (benchmark-time 0.0))

  (printout t "FBM: Start" crlf)
  (assert (attention-message (text "FBM: Start") (time 15)))
)

; The current run has timed-out, switch to "run-over" state
(defrule benchmark-fbm-run-timeout
  (benchmark-phase (id ?phase) (type FBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING) (run ?run)
           (max-time ?max-time) (benchmark-time ?benchmark-time&:(>= ?benchmark-time ?max-time)))
  =>
  (assert (benchmark-run-over))
)

; Still runs left, so loop
(defrule benchmark-fbm-loop
  ?ro <- (benchmark-run-over)
  (benchmark-phase (id ?phase) (type FBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING)
           (max-runs ?max-runs) (run ?run&:(< ?run ?max-runs)))
  =>
  (retract ?ro)   ; Reset for the next run
  (modify ?bs (state PAUSED) (prev-state RUNNING) (end-time (now)) (run (+ ?run 1)) (benchmark-time 0.0))

  (printout t "FBM: Run " ?run " over" crlf)
  (assert (attention-message (text (str-cat "FBM: Run " ?run " over")) (time 15)))
)

; All runs over, so finish
(defrule benchmark-fbm-over
  ?ro <- (benchmark-run-over)
  (benchmark-phase (id ?phase) (type FBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING)
           (max-runs ?max-runs) (run ?run&:(>= ?run ?max-runs)))
  =>
  (retract ?ro)   ; Reset for future runs
  (modify ?bs (state FINISHED) (prev-state RUNNING))

  (printout t "FBM: Benchmark over" crlf)
  (assert (attention-message (text "FBM: Benchmark over") (time 15)))
)



(deffunction benchmark-tbm1-init ()
  ; Inventory
  (assert
    ;;;;;;;;;;;;;;;;;;;;
    ; Assembly aid trays
    ;;;;;;;;;;;;;;;;;;;;

    ; Assembly aid tray EM-01-02 (object-id 21) at shelf SH-08 (location-id 8)
    (item (id 1) (object-id 21) (location-id 8))


    ;;;;;;;;;;;;;;;
    ; Bearing boxes
    ;;;;;;;;;;;;;;;

    ; 1 bearing box (object-id 1) at shelf SH-02 (location-id 2)
    (item (id 10) (object-id 1) (location-id 2) (quantity 1))

    ; 1 bearing box (object-id 1) at shelf SH-20 (location-id 20)
    (item (id 13) (object-id 1) (location-id 20) (quantity 1))
  )


  ; Orders
  (assert
    ; Deliver 2 items of AX-01 (object-id 1) to EM-01-02 (container-id 21)
    (order (id 1) (status OFFERED) (object-id 1) (container-id 21) (quantity-requested 2))

    ; Deliver container EM-01-02 (object-id 21) to location WS-01 (location-id 30)
    (order (id 2) (status OFFERED) (object-id 21) (destination-id 30))
  )
)


(deffunction benchmark-tbm2-init ()
  ; Inventory
  (assert
    ;;;;;;;;;;;;;;
    ; Cover plates
    ;;;;;;;;;;;;;;

    ; 5 cover plates with unknown state (object-id 15) on conveyor belt CB-01 (location-id 32)
    (item (id 1) (object-id 15) (location-id 40) (quantity 1))
    (item (id 2) (object-id 15) (location-id 40) (quantity 1))
    (item (id 3) (object-id 15) (location-id 40) (quantity 1))
    (item (id 4) (object-id 15) (location-id 40) (quantity 1))
    (item (id 5) (object-id 15) (location-id 40) (quantity 1))

    ;;;;;;;;;;;;;;;;;;;;;;
    ; Manipulation objects
    ;;;;;;;;;;;;;;;;;;;;;;

    ; File-card box EM-02-02 (object-id 32) in shelf SH-14 (location-id 14)
    (item (id 6) (object-id 32) (location-id 14))

    ; File-card box EM-02-02 (object-id 32) on robot (location-id 50)
    (item (id 6) (object-id 32) (location-id 50))

    ; Common shelf container ER-02-04 (object-id 61) in location CB-01 (location-id 40)
    (item (id 7) (object-id 64) (location-id 40))
  )


  ; Orders
  (assert
    ; Deliver file-card box EM-02-02 (container-id 32) to workstation WS-05 (destination-id 34)
    (order (id  1) (status OFFERED) (object-id 32) (destination-id 34))

    ; Deliver 5 machined cover plates AX-07 (object-id 7) into file-card box EM-02-02 (container-id 32)
    (order (id  2) (status OFFERED) (object-id 7) (container-id 32) (quantity-requested 1))
    (order (id  3) (status OFFERED) (object-id 7) (container-id 32) (quantity-requested 1))
    (order (id  4) (status OFFERED) (object-id 7) (container-id 32) (quantity-requested 1))
    (order (id  5) (status OFFERED) (object-id 7) (container-id 32) (quantity-requested 1))
    (order (id  6) (status OFFERED) (object-id 7) (container-id 32) (quantity-requested 1))

    ; Deliver 5 unusable cover plates AX-08 (object-id 8) into common shelf container ER-02-04 (container-id 64)
    (order (id  7) (status OFFERED) (object-id 8) (container-id 64) (quantity-requested 1))
    (order (id  8) (status OFFERED) (object-id 8) (container-id 64) (quantity-requested 1))
    (order (id  9) (status OFFERED) (object-id 8) (container-id 64) (quantity-requested 1))
    (order (id 10) (status OFFERED) (object-id 8) (container-id 64) (quantity-requested 1))
    (order (id 11) (status OFFERED) (object-id 8) (container-id 64) (quantity-requested 1))
  )
)


(deffunction benchmark-tbm3-init ()
  ; Inventory
  (assert
    ;;;;;;;;;;
    ; Objects
    ;;;;;;;;;;

    ; 1 bearing box AX-01 (object-id 1) at shelf SH-02 (location-id 2)
    (item (id 1) (object-id 1) (location-id 2) (quantity 1))

    ; 1 bearing AX-02 (object-id 2) at shelf SH-07 (location-id 7)
    (item (id 2) (object-id 2) (location-id 7) (quantity 1))

    ; 1 axis AX-03 (object-id 3) at shelf SH-09 (location-id 9)
    (item (id 3) (object-id 3) (location-id 9) (quantity 1))

    ; 1 shaft nut AX-04 (object-id 4) at shelf SH-13 (location-id 13)
    (item (id 4) (object-id 4) (location-id 13) (quantity 1))

    ; 1 distance tube AX-05 (object-id 5) at shelf SH-15 (location-id 15)
    (item (id 5) (object-id 5) (location-id 15) (quantity 1))

    ; 1 motor with gear box AX-09 (object-id 9) at shelf SH-21 (location-id 21)
    (item (id 7) (object-id 9) (location-id 21) (quantity 1))

    ;;;;;;;;;;;;;;;;
    ; Foam container
    ;;;;;;;;;;;;;;;;

    ; Foam container EM-03-01 (object-id 41) at shelf SH-01 (location-id 1)
    (item (id 20) (object-id 41) (location-id 1))
  )


  ; Orders
  (assert
    ; Deliver foam container EM-03-01 (object-id 41) to workstation WS-04 (location-id 33)
    (order (id 1) (status OFFERED) (object-id 41) (destination-id 33))

    ; Deliver 1 bearing box AX-01 (object-id 1) into foam container EM-03-01 (container-id 41)
    (order (id 2) (status OFFERED) (object-id 7) (container-id 41) (quantity-requested 1))

    ; Deliver 1 bearing AX-02 (object-id 2) into foam container EM-03-01 (container-id 41)
    (order (id 3) (status OFFERED) (object-id 2) (container-id 41) (quantity-requested 1))

    ; Deliver 1 axis AX-03 (object-id 3) into foam container EM-03-01 (container-id 41)
    (order (id 4) (status OFFERED) (object-id 3) (container-id 41) (quantity-requested 1))

    ; Deliver 1 shaft nut AX-04 (object-id 4) into foam container EM-03-01 (container-id 41)
    (order (id 5) (status OFFERED) (object-id 4) (container-id 41) (quantity-requested 1))

    ; Deliver 1 distance tube AX-05 (object-id 5) into foam container EM-03-01 (container-id 41)
    (order (id 6) (status OFFERED) (object-id 5) (container-id 41) (quantity-requested 1))

    ; Deliver 1 motor with gear box AX-09 (object-id 9) into foam container EM-03-01 (container-id 41)
    (order (id 7) (status OFFERED) (object-id 9) (container-id 41) (quantity-requested 1))
  )
)



; Initialize and directly transition to PAUSED state
(defrule benchmark-tbm-init
  (benchmark-phase (id ?phase) (type TBM) (type-id ?fbm-id))
  ?bs <- (benchmark-state (phase-id ?phase) (state INIT))
  =>
  (benchmark-reset)

  (switch ?fbm-id
    (case 1 then
      (benchmark-tbm1-init)
    )
    (case 2 then
      (benchmark-tbm2-init)
    )
    (case 3 then
      (benchmark-tbm3-init)
    )
  )

  (modify ?bs (state PAUSED) (prev-state INIT) (max-runs ?*TBM-COUNT*)
      (max-time ?*TBM-TIME*) (run 1) (benchmark-time 0.0))
)

; When a command switches the state from PAUSED to RUNNING, setup the current run
(defrule benchmark-tbm-run-start
  (benchmark-phase (id ?phase) (type TBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING) (prev-state PAUSED))
  =>
  (modify ?bs (prev-state RUNNING) (start-time (now)) (benchmark-time 0.0))

  (printout t "TBM: Start" crlf)
  (assert (attention-message (text "TBM: Start") (time 15)))
)

; The current run has timed-out, switch to "run-over" state
(defrule benchmark-tbm-run-timeout
  (benchmark-phase (id ?phase) (type TBM))
  ?bs <- (benchmark-state (phase-id ?phase) (state RUNNING)
           (max-time ?max-time) (benchmark-time ?benchmark-time&:(>= ?benchmark-time ?max-time)))
  =>
  (modify ?bs (state FINISHED) (prev-state RUNNING))

  (printout t "TBM: Benchmark over" crlf)
  (assert (attention-message (text "TBM: Benchmark over") (time 15)))
)
