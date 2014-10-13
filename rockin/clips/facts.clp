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

  ; cardinality 2: sec msec
  (multislot start-time (type INTEGER) (cardinality 2 2) (default 0 0))
  (multislot end-time (type INTEGER) (cardinality 2 2) (default 0 0))
  (multislot last-time (type INTEGER) (cardinality 2 2) (default 0 0))
)


(deffacts startup
  (signal (type version-info) (time (create$ 0 0)) (seq 1))
  (signal (type beacon) (time (create$ 0 0)) (seq 1))
  (signal (type benchmark-state) (time (create$ 0 0)) (seq 1))
)
