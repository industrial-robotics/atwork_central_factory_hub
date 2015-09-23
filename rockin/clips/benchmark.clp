;---------------------------------------------------------------------------
;  benchmark.clp - RoCKIn RefBox CLIPS benchmark
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BenchmarkPhase (is-a USER)
  ; NONE: No benchmark running
  ; TBM: task benchmark
  ; FBM: functionality benchmark
  (slot type (type SYMBOL) (allowed-values NONE TBM FBM) (default NONE))
  (slot type-id (type INTEGER) (default 0))
)

(defmessage-handler BenchmarkPhase create-msg ()
  "Create a ProtoBuf message for a benchmark phase description"

  (bind ?pb-benchmark-phase (pb-create "rockin_msgs.BenchmarkPhase"))
 
  (pb-set-field ?pb-benchmark-phase "type" ?self:type)
  (pb-set-field ?pb-benchmark-phase "type_id" ?self:type-id)

  (do-for-fact ((?phase benchmark-phase)) (and (eq ?phase:type ?self:type) (eq ?phase:type-id ?self:type-id))
    (pb-set-field ?pb-benchmark-phase "description" ?phase:description)
  )

  (return ?pb-benchmark-phase)
)


(defclass Benchmark (is-a USER)
  (slot current-phase (type INSTANCE) (allowed-classes BenchmarkPhase))
  (slot requested-phase (type INSTANCE) (allowed-classes BenchmarkPhase))

  ; time that the benchmark is running
  (slot time (type INSTANCE) (allowed-classes BenchmarkTime))
)

(defmessage-handler Benchmark switch-phase ()
  (bind ?self:current-phase ?self:requested-phase)

  (bind ?phase-type (send ?self:current-phase get-type))
  (bind ?phase-type-id (send ?self:current-phase get-type-id))


  ; Remove all items from the inventory
  (foreach ?item (send [inventory] get-items)
    (unmake-instance ?item)
  )
  (slot-delete$ [inventory] items 1 (length$ (send [inventory] get-items)))

  ; Remove all orders from the order info
  (foreach ?order (send [order-info] get-orders)
    (unmake-instance ?order)
  )
  (slot-delete$ [order-info] orders 1 (length$ (send [order-info] get-orders)))


  (if (and (eq ?phase-type FBM) (eq ?phase-type-id 1)) then (functionality-benchmarks-fbm1-init ?self:time))
  (if (and (eq ?phase-type FBM) (eq ?phase-type-id 2)) then (functionality-benchmarks-fbm2-init ?self:time))
  (if (and (eq ?phase-type TBM) (eq ?phase-type-id 1)) then (task-benchmarks-tbm1-init ?self:time))
  (if (and (eq ?phase-type TBM) (eq ?phase-type-id 2)) then (task-benchmarks-tbm2-init ?self:time))
  (if (and (eq ?phase-type TBM) (eq ?phase-type-id 3)) then (task-benchmarks-tbm3-init ?self:time))
)

(defmessage-handler Benchmark set-requested-phase (?type ?type-id)
  (send ?self:requested-phase put-type ?type)
  (send ?self:requested-phase put-type-id ?type-id)
)


(defrule init-benchmark
  (init)
  =>
  (make-instance [benchmark] of Benchmark
    (current-phase (make-instance of BenchmarkPhase))
    (requested-phase (make-instance of BenchmarkPhase))
    (time (make-instance of BenchmarkTime))
  )
)
