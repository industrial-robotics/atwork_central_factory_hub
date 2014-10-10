;---------------------------------------------------------------------------
;  facts.clp - RoCKIn RefBox CLIPS - facts specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

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


(deffacts startup
  (signal (type version-info) (time (create$ 0 0)) (seq 1))
  (signal (type beacon) (time (create$ 0 0)) (seq 1))
)
