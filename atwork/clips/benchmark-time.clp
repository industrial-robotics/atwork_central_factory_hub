;---------------------------------------------------------------------------
;  benchmark-time.clp - RoCKIn RefBox CLIPS benchmark timer
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BenchmarkTime (is-a USER) (role concrete)
  (slot timer (type FLOAT) (default 0.0))

  ; cardinality 2: sec msec
  (multislot start-time (type INTEGER) (cardinality 2 2) (default 0 0))
  (multislot end-time (type INTEGER) (cardinality 2 2) (default 0 0))
)
