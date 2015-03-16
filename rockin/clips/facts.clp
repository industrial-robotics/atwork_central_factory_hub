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

  (slot max-time (type FLOAT) (default 0.0))
)

(deftemplate benchmark-feedback
  (slot source (type SYMBOL))
  (multislot time (type INTEGER) (cardinality 2 2) (default (create$ 0 0)))
  (slot type (type SYMBOL) (allowed-values SELECTED STARTED TIMEOUT RECOGNIZED LIFTED FAIL SUCCESS))

  (slot object-class-name (type STRING) (default ""))
  (slot object-pose-position-x (type FLOAT) (default 0.0))
  (slot object-pose-position-y (type FLOAT) (default 0.0))
  (slot object-pose-position-z (type FLOAT) (default 0.0))
  (slot object-pose-orientation-w (type FLOAT) (default 0.0))
  (slot object-pose-orientation-x (type FLOAT) (default 0.0))
  (slot object-pose-orientation-y (type FLOAT) (default 0.0))
  (slot object-pose-orientation-z (type FLOAT) (default 0.0))
  (slot object-instance-name (type STRING) (default ""))
  (slot grasp-notification (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
  (slot end-effector-pose-position-x (type FLOAT) (default 0.0))
  (slot end-effector-pose-position-y (type FLOAT) (default 0.0))
  (slot end-effector-pose-position-z (type FLOAT) (default 0.0))
  (slot end-effector-pose-orientation-w (type FLOAT) (default 0.0))
  (slot end-effector-pose-orientation-x (type FLOAT) (default 0.0))
  (slot end-effector-pose-orientation-y (type FLOAT) (default 0.0))
  (slot end-effector-pose-orientation-z (type FLOAT) (default 0.0))
)


(deffacts startup
  (signal (type version-info) (time (create$ 0 0)) (seq 1))
  (signal (type beacon) (time (create$ 0 0)) (seq 1))
  (signal (type benchmark-state) (time (create$ 0 0)) (seq 1))
  (signal (type robot-info) (time (create$ 0 0)) (seq 1))
  (signal (type order-info) (time (create$ 0 0)) (seq 1))
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
