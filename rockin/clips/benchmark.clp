;---------------------------------------------------------------------------
;  benchmark.clp - RoCKIn RefBox CLIPS benchmark
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BenchmarkScenario (is-a USER)
  ; NONE: No benchmark running
  ; TBM: task benchmark
  ; FBM: functionality benchmark
  (slot type (type SYMBOL) (allowed-values NONE TBM FBM) (default NONE))
  (slot type-id (type INTEGER) (default 0))
)

(defmessage-handler BenchmarkScenario create-msg ()
  "Create a ProtoBuf message for a benchmark scenario description"

  (bind ?pb-benchmark-scenario (pb-create "rockin_msgs.BenchmarkPhase"))
 
  (pb-set-field ?pb-benchmark-scenario "type" ?self:type)
  (pb-set-field ?pb-benchmark-scenario "type_id" ?self:type-id)

  (do-for-fact ((?scenario benchmark-scenario)) (and (eq ?scenario ?self:type) (eq ?scenario:type-id ?self:type-id))
    (pb-set-field ?pb-benchmark-scenario "description" ?scenario)
  )

  (return ?pb-benchmark-scenario)
)


(defclass Benchmark (is-a USER)
  (slot current-scenario (type INSTANCE) (allowed-classes BenchmarkScenario))
  (slot requested-scenario (type INSTANCE) (allowed-classes BenchmarkScenario))

  ; time that the benchmark is running
  (slot time (type INSTANCE) (allowed-classes BenchmarkTime))

  ; State machine that coordinates the benchmark execution
  (slot state-machine (type INSTANCE) (allowed-classes StateMachine))
)

(defmessage-handler Benchmark switch-scenario ()
  (bind ?self:current-scenario ?self:requested-scenario)

  (bind ?scenario-type (send ?self:current-scenario get-type))
  (bind ?scenario-type-id (send ?self:current-scenario get-type-id))


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


  ; TODO: Remove all states and their transitions

  (if (and (eq ?scenario-type FBM) (eq ?scenario-type-id 1)) then
    (functionality-benchmarks-fbm1-init ?self:time ?self:state-machine)
  )
  (if (and (eq ?scenario-type FBM) (eq ?scenario-type-id 2)) then
    (functionality-benchmarks-fbm2-init ?self:time ?self:state-machine)
  )
  (if (and (eq ?scenario-type TBM) (eq ?scenario-type-id 1)) then
    (task-benchmarks-tbm1-init ?self:time ?self:state-machine)
  )
  (if (and (eq ?scenario-type TBM) (eq ?scenario-type-id 2)) then
    (task-benchmarks-tbm2-init ?self:time ?self:state-machine)
  )
  (if (and (eq ?scenario-type TBM) (eq ?scenario-type-id 3)) then
    (task-benchmarks-tbm3-init ?self:time ?self:state-machine)
  )
)

(defmessage-handler Benchmark set-requested-scenario (?type ?type-id)
  (send ?self:requested-scenario put-type ?type)
  (send ?self:requested-scenario put-type-id ?type-id)
)


(defrule init-benchmark
  (init)
  =>
  (make-instance [init-state] of InitState)

  (make-instance [benchmark] of Benchmark
    (current-scenario (make-instance of BenchmarkScenario))
    (requested-scenario (make-instance of BenchmarkScenario))
    (time (make-instance of BenchmarkTime))
    (state-machine
      (make-instance of StateMachine
        (current-state [init-state])
        (states [init-state])
      )
    )
  )
)

(defrule benchmark-update
  (time $?now)
  ?bm <- (object (is-a Benchmark))
  =>
  (bind ?state-machine (send ?bm get-state-machine))
  (send ?state-machine update)
)
