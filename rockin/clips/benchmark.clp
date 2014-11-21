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


(defrule benchmark-switch-to-init
  ?bs <- (benchmark-state (state INIT) (prev-state ~INIT))
  =>
  (benchmark-reset)
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



(defrule benchmark-tbm1-init
  (benchmark-phase (id ?phase) (type TBM) (type-id 1))
  ?bs <- (benchmark-state (phase-id ?phase) (state INIT))
  (not (benchmark-initialized))
  =>
  (assert (benchmark-initialized))
  (benchmark-reset)

  ; Inventory
  (assert
    ;;;;;;;;;;;;;;;;;;;;
    ; Assembly aid trays
    ;;;;;;;;;;;;;;;;;;;;

    ; Assembly aid tray EM-01-01 (object-id 20) at workstation WS-01 (location-id 30)
    (item (id 1) (object-id 20) (location-id 30))

    ; Assembly aid tray EM-01-02 (object-id 21) at workstation WS-01 (location-id 30)
    (item (id 2) (object-id 21) (location-id 30))

    ; Assembly aid tray EM-01-03 (object-id 22) at workstation WS-03 (location-id 32)
    (item (id 3) (object-id 22) (location-id 32))


    ;;;;;;;;;;;;;;;
    ; Bearing boxes
    ;;;;;;;;;;;;;;;

    ; 1 bearing box (object-id 1) at shelf SH-08 (location-id 8)
    (item (id 10) (object-id 1) (location-id 8) (quantity 1))

    ; 1 bearing box (object-id 1) at shelf SH-14 (location-id 14)
    (item (id 11) (object-id 1) (location-id 14) (quantity 1))

    ; 1 bearing box (object-id 1) at shelf SH-19 (location-id 19)
    (item (id 12) (object-id 1) (location-id 19) (quantity 1))

    ; 1 bearing box (object-id 1) at shelf SH-21 (location-id 21)
    (item (id 13) (object-id 1) (location-id 21) (quantity 1))

    ; 2 bearing boxes (object-id 1) at shelf SH-02 (location-id 2)
    (item (id 14) (object-id 1) (location-id 2) (quantity 1))
    (item (id 15) (object-id 1) (location-id 2) (quantity 1))
  )


  ; Orders
  (assert
    ; Deliver 2 items of AX-01 (object-id 1) to EM-01-01 (container 20)
    (order (id 1) (status OFFERED) (object-id 1) (container-id 20) (quantity-requested 2))

    ; Deliver 2 items of AX-01 (object-id 1) to EM-01-02 (container 21)
    (order (id 2) (status OFFERED) (object-id 1) (container-id 21) (quantity-requested 2))

    ; Deliver 2 items of AX-01 (object-id 1) to EM-01-03 (container 22)
    (order (id 3) (status OFFERED) (object-id 1) (container-id 22) (quantity-requested 2))
  )
)


(defrule benchmark-tbm2-init
  (benchmark-phase (id ?phase) (type TBM) (type-id 2))
  ?bs <- (benchmark-state (phase-id ?phase) (state INIT))
  (not (benchmark-initialized))
  =>
  (assert (benchmark-initialized))
  (benchmark-reset)

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
  )


  ; Orders
  (assert
    ; Deliver 5 machined cover plates AX-07 (object-id 7) in file-card box EM-02-01 (container-id 31) to workstation WS-04 (destination-id 33)
    (order (id 1) (status OFFERED) (object-id 7) (container-id 31) (quantity-requested 5) (destination-id 33))
  )
)


(defrule benchmark-tbm3-init
  (benchmark-phase (id ?phase) (type TBM) (type-id 3))
  ?bs <- (benchmark-state (phase-id ?phase) (state INIT))
  (not (benchmark-initialized))
  =>
  (assert (benchmark-initialized))
  (benchmark-reset)

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

    ; 1 machined cover plate AX-07 (object-id 7) in container EM-02-01 (container-id 31)
    (item (id 6) (object-id 7) (container-id 31) (quantity 1))

    ; 1 motor with gear box AX-09 (object-id 9) at shelf SH-21 (location-id 21)
    (item (id 7) (object-id 9) (location-id 21) (quantity 1))

    ; File card box EM-02-01 (object-id 31) at shelf SH-19 (location-id 19)
    (item (id 8) (object-id 31) (location-id 19))

    ;;;;;;;;;;;;;;;;
    ; Foam container
    ;;;;;;;;;;;;;;;;

    ; Foam container EM-03-01 (object-id 41) at workstation WS-04 (location-id 33)
    (item (id 20) (object-id 41) (location-id 33))
  )


  ; Orders
  (assert
    ; Deliver 5 machined cover plates AX-07 (object-id 7) in file-card box EM-02-01 (container-id 31) to workstation WS-04 (destination-id 33)
    (order (id 1) (status OFFERED) (object-id 7) (container-id 31) (quantity-requested 5) (destination-id 33))
  )
)