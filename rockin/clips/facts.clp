;---------------------------------------------------------------------------
;  facts.clp - RoCKIn RefBox CLIPS - facts specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deftemplate known-team
  (slot name (type STRING))
)

(deftemplate robot
  (slot team (type STRING))
  (slot name (type STRING))
  (slot host (type STRING))
  (slot port (type INTEGER))
  (multislot last-seen (type INTEGER) (cardinality 2 2))
  (slot warning-sent (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
)

(deftemplate signal
  (slot type)
  (multislot time (type INTEGER) (cardinality 2 2) (default (create$ 0 0)))
  (slot seq (type INTEGER) (default 1))
  (slot count (type INTEGER) (default 1))
)

(deftemplate network-client
  (slot id (type INTEGER))
  (slot host (type STRING))
  (slot port (type INTEGER))
)

(deftemplate network-peer
  (slot group (type STRING))
  (slot id (type INTEGER))
)

(deftemplate attention-message
  (slot team (type STRING) (default ""))
  (slot text (type STRING))
  (slot time (type INTEGER) (default 5))
)

(deftemplate order
  (slot id (type INTEGER))
  (slot status (type SYMBOL) (allowed-values OFFERED TIMEOUT IN_PROGRESS PAUSED ABORTED FINISHED))
  (slot object-id (type INTEGER))                           ; id of an object identifier
  (multislot container (type INTEGER) (cardinality 0 1))    ; id of an object identifier
  (slot quantity-delivered (type INTEGER) (default 0))
  (multislot quantity-requested (type INTEGER) (cardinality 0 1))
  (multislot destination (type INTEGER) (cardinality 0 1))  ; id of a location identifier
  (multislot source (type INTEGER) (cardinality 0 1))       ; id of a location identifier
  (multislot processing-team (type STRING) (cardinality 0 1))
)

(deftemplate benchmark-phase
  (slot id (type INTEGER))
  (slot type (type SYMBOL) (allowed-values NONE TBM FBM))  ; No benchmark running, task benchmark, functionality benchmark
  (slot type-id (type INTEGER))
  (slot description (type STRING))
)

(deftemplate benchmark-state
  (slot refbox-mode (type SYMBOL) (allowed-values STANDALONE) (default STANDALONE))
  (slot state (type SYMBOL) (allowed-values INIT RUNNING PAUSED FINISHED) (default INIT))
  (slot prev-state (type SYMBOL) (allowed-values INIT RUNNING PAUSED FINISHED) (default INIT))
  (slot phase-id (type INTEGER) (default 0))        ; identifier of a phase
  (slot prev-phase-id (type INTEGER) (default 0))   ; identifier of a phase
  (slot benchmark-time (type FLOAT) (default 0.0))

  ; cardinality 2: sec msec
  (multislot start-time (type INTEGER) (cardinality 2 2) (default 0 0))
  (multislot end-time (type INTEGER) (cardinality 2 2) (default 0 0))
  (multislot last-time (type INTEGER) (cardinality 2 2) (default 0 0))
)

(deftemplate object-identifier
  ; identifier which is used in CLIPS only
  (slot id (type INTEGER))

  ; the object id (see rulebook), such as AX-01 consists of *type* and *type-id*
  (slot type (type SYMBOL) (allowed-values EM AX ER))
  (slot type-id (type INTEGER))

  (multislot instance-id (type INTEGER) (cardinality 0 1))
  (multislot description (type STRING) (cardinality 0 1))
)

(deftemplate location-identifier
  ; identifier which is used in CLIPS only
  (slot id (type INTEGER))

  ; a location id, such as SHELF-01 consists of *type* and *instance-id*
  (slot type (type SYMBOL) (allowed-values SH WS CB ROBOT))
  (slot instance-id (type INTEGER))

  (multislot description (type STRING) (cardinality 0 1))
)

(deftemplate item
  ; identifier which is used in CLIPS only
  (slot id (type INTEGER))

  (slot object-id (type INTEGER))                           ; object-identifier
  (multislot container-id (type INTEGER) (cardinality 0 1)) ; object-identifier
  (multislot location-id (type INTEGER) (cardinality 0 1))  ; location-identifier
  (multislot quantity (type INTEGER) (cardinality 0 1))
)


(deffacts startup
  (signal (type version-info) (time (create$ 0 0)) (seq 1))
  (signal (type beacon) (time (create$ 0 0)) (seq 1))
  (signal (type benchmark-state) (time (create$ 0 0)) (seq 1))
  (signal (type inventory) (time (create$ 0 0)) (seq 1))
  (signal (type drilling-machine) (time (create$ 0 0)) (seq 1))
  (signal (type conveyor-belt) (time (create$ 0 0)) (seq 1))
)

(deffacts benchmarks
  (benchmark-phase (id 0) (type NONE) (type-id 0) (description "No benchmark running"))
  (benchmark-phase (id 1) (type TBM)  (type-id 1) (description "Prepare Assembly Aid Tray for Force Fitting"))
  (benchmark-phase (id 2) (type TBM)  (type-id 2) (description "Plate Drilling"))
  (benchmark-phase (id 3) (type TBM)  (type-id 3) (description "Fill a Box with Parts for Manual Assembly"))
  (benchmark-phase (id 4) (type FBM)  (type-id 1) (description "Object Perception Functionality"))
  (benchmark-phase (id 5) (type FBM)  (type-id 2) (description "Visual Servoing Functionality"))

  (benchmark-state)
)


(deffacts inventory
  ;;;;;;;;;;;
  ; Locations
  ;;;;;;;;;;;

  ; Shelves
  (location-identifier (id  1) (type SH) (instance-id  1) (description "SHELF-01"))
  (location-identifier (id  2) (type SH) (instance-id  2) (description "SHELF-02"))
  (location-identifier (id  3) (type SH) (instance-id  3) (description "SHELF-03"))
  (location-identifier (id  4) (type SH) (instance-id  4) (description "SHELF-04"))
  (location-identifier (id  5) (type SH) (instance-id  5) (description "SHELF-05"))
  (location-identifier (id  6) (type SH) (instance-id  6) (description "SHELF-06"))
  (location-identifier (id  7) (type SH) (instance-id  7) (description "SHELF-07"))
  (location-identifier (id  8) (type SH) (instance-id  8) (description "SHELF-08"))
  (location-identifier (id  9) (type SH) (instance-id  9) (description "SHELF-09"))
  (location-identifier (id 10) (type SH) (instance-id 10) (description "SHELF-10"))
  (location-identifier (id 11) (type SH) (instance-id 11) (description "SHELF-11"))
  (location-identifier (id 12) (type SH) (instance-id 12) (description "SHELF-12"))
  (location-identifier (id 13) (type SH) (instance-id 13) (description "SHELF-13"))
  (location-identifier (id 14) (type SH) (instance-id 14) (description "SHELF-14"))
  (location-identifier (id 15) (type SH) (instance-id 15) (description "SHELF-15"))
  (location-identifier (id 16) (type SH) (instance-id 16) (description "SHELF-16"))
  (location-identifier (id 17) (type SH) (instance-id 17) (description "SHELF-17"))
  (location-identifier (id 18) (type SH) (instance-id 18) (description "SHELF-18"))
  (location-identifier (id 19) (type SH) (instance-id 19) (description "SHELF-19"))
  (location-identifier (id 20) (type SH) (instance-id 20) (description "SHELF-20"))
  (location-identifier (id 21) (type SH) (instance-id 21) (description "SHELF-21"))
  (location-identifier (id 22) (type SH) (instance-id 22) (description "SHELF-22"))
  (location-identifier (id 23) (type SH) (instance-id 23) (description "SHELF-23"))
  (location-identifier (id 24) (type SH) (instance-id 24) (description "SHELF-24"))

  ; Workstations
  (location-identifier (id 25) (type WS) (instance-id 1) (description "WORKSTATION-01"))
  (location-identifier (id 26) (type WS) (instance-id 2) (description "WORKSTATION-02"))
  (location-identifier (id 27) (type WS) (instance-id 3) (description "WORKSTATION-03"))
  (location-identifier (id 28) (type WS) (instance-id 4) (description "WORKSTATION-04"))
  (location-identifier (id 29) (type WS) (instance-id 5) (description "WORKSTATION-05"))
  (location-identifier (id 30) (type WS) (instance-id 6) (description "WORKSTATION-06"))
  (location-identifier (id 31) (type WS) (instance-id 7) (description "WORKSTATION-07"))

  ; Conveyor belt
  (location-identifier (id 32) (type CB) (instance-id 1) (description "CONVEYOR_BELT-01"))

  ; Robot
  (location-identifier (id 33) (type ROBOT) (instance-id 1) (description "ROBOT"))


  ;;;;;;;;;;;;;;;;;;;;
  ; Object identifiers
  ;;;;;;;;;;;;;;;;;;;;

  ; Bearing box (class)
  (object-identifier (id  1) (type AX) (type-id 1) (description "AX-01"))

  ; Bearing (class)
  (object-identifier (id  2) (type AX) (type-id 2) (description "AX-02"))

  ; Axis (class)
  (object-identifier (id  3) (type AX) (type-id 3) (description "AX-03"))

  ; Shaft nut (class)
  (object-identifier (id  4) (type AX) (type-id 4) (description "AX-04"))

  ; Distance tube (class)
  (object-identifier (id  5) (type AX) (type-id 5) (description "AX-05"))

  ; Cover plate - defect (class)
  (object-identifier (id  6) (type AX) (type-id 6) (description "AX-06"))

  ; Cover plate - machined (class)
  (object-identifier (id  7) (type AX) (type-id 7) (description "AX-07"))

  ; Cover plate - faulty (class)
  (object-identifier (id  8) (type AX) (type-id 8) (description "AX-08"))

  ; Motor with gearbox (class)
  (object-identifier (id  9) (type AX) (type-id 9) (description "AX-09"))


  ; Assembly aid trays (instances)
  (object-identifier (id 10) (type EM) (type-id 1) (instance-id 1) (description "EM-01-01"))
  (object-identifier (id 11) (type EM) (type-id 1) (instance-id 2) (description "EM-01-02"))
  (object-identifier (id 12) (type EM) (type-id 1) (instance-id 3) (description "EM-01-03"))

  ; File card box (instances)
  (object-identifier (id 13) (type EM) (type-id 2) (instance-id 1) (description "EM-02-01"))
  (object-identifier (id 14) (type EM) (type-id 2) (instance-id 2) (description "EM-02-02"))
  (object-identifier (id 15) (type EM) (type-id 2) (instance-id 3) (description "EM-02-03"))

  ; Foam container (instances)
  (object-identifier (id 16) (type EM) (type-id 3) (instance-id 1) (description "EM-03-01"))

  ; Tray rack (instances)
  (object-identifier (id 17) (type ER) (type-id 1) (instance-id 1) (description "ER-01-01"))

  ; Common shelf container (instances)
  (object-identifier (id 18) (type ER) (type-id 2) (instance-id  1) (description "ER-02-01"))
  (object-identifier (id 19) (type ER) (type-id 2) (instance-id  2) (description "ER-02-02"))
  (object-identifier (id 20) (type ER) (type-id 2) (instance-id  3) (description "ER-02-03"))
  (object-identifier (id 21) (type ER) (type-id 2) (instance-id  4) (description "ER-02-04"))
  (object-identifier (id 22) (type ER) (type-id 2) (instance-id  5) (description "ER-02-05"))
  (object-identifier (id 23) (type ER) (type-id 2) (instance-id  6) (description "ER-02-06"))
  (object-identifier (id 24) (type ER) (type-id 2) (instance-id  7) (description "ER-02-07"))
  (object-identifier (id 25) (type ER) (type-id 2) (instance-id  8) (description "ER-02-08"))
  (object-identifier (id 26) (type ER) (type-id 2) (instance-id  9) (description "ER-02-09"))
  (object-identifier (id 27) (type ER) (type-id 2) (instance-id 10) (description "ER-02-10"))
)

