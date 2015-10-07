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
  (slot description (type STRING) (default ""))
)

(defmessage-handler BenchmarkScenario create-msg ()
  "Create a ProtoBuf message for a benchmark scenario description"

  (bind ?pb-benchmark-scenario (pb-create "rockin_msgs.BenchmarkPhase"))
 
  (pb-set-field ?pb-benchmark-scenario "type" ?self:type)
  (pb-set-field ?pb-benchmark-scenario "type_id" ?self:type-id)
  (pb-set-field ?pb-benchmark-scenario "description" ?self:description)

  (return ?pb-benchmark-scenario)
)

(defmessage-handler BenchmarkScenario setup (?time ?state-machine)
)


(defclass Benchmark (is-a USER)
  (slot current-scenario (type INSTANCE) (allowed-classes BenchmarkScenario))
  (slot requested-scenario (type INSTANCE) (allowed-classes BenchmarkScenario))
  (multislot registered-scenarios (type INSTANCE) (allowed-classes BenchmarkScenario))

  ; time that the benchmark is running
  (slot time (type INSTANCE) (allowed-classes BenchmarkTime))

  ; State machine that coordinates the benchmark execution
  (slot state-machine (type INSTANCE) (allowed-classes StateMachine))
)

(defmessage-handler Benchmark switch-scenario ()
  (bind ?self:current-scenario ?self:requested-scenario)


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

  (if (eq ?self:current-scenario [FBM1]) then
    (functionality-benchmarks-fbm1-init ?self:time ?self:state-machine)
  )
  (if (eq ?self:current-scenario [FBM2]) then
    (functionality-benchmarks-fbm2-init ?self:time ?self:state-machine)
  )
  (if (eq ?self:current-scenario [TBM1]) then
    (task-benchmarks-tbm1-init ?self:time ?self:state-machine)
  )
  (if (eq ?self:current-scenario [TBM2]) then
    (task-benchmarks-tbm2-init ?self:time ?self:state-machine)
  )
  (if (eq ?self:current-scenario [TBM3]) then
    (task-benchmarks-tbm3-init ?self:time ?self:state-machine)
  )
)

(defmessage-handler Benchmark request-scenario (?type ?type-id)
  (send ?self:requested-scenario put-type ?type)
  (send ?self:requested-scenario put-type-id ?type-id)

  (if (and (eq ?type FBM) (eq ?type-id 1)) then (send ?self put-requested-scenario [FBM1]))
  (if (and (eq ?type FBM) (eq ?type-id 2)) then (send ?self put-requested-scenario [FBM2]))
  (if (and (eq ?type TBM) (eq ?type-id 1)) then (send ?self put-requested-scenario [TBM1]))
  (if (and (eq ?type TBM) (eq ?type-id 2)) then (send ?self put-requested-scenario [TBM2]))
  (if (and (eq ?type TBM) (eq ?type-id 3)) then (send ?self put-requested-scenario [TBM3]))
)


(defrule init-benchmark
  (init)
  =>
  (make-instance [NONE] of BenchmarkScenario (type NONE) (type-id 0) (description "No benchmark running"))
  (make-instance [TBM1] of BenchmarkScenario (type TBM)  (type-id 1) (description "Prepare Assembly Aid Tray for Force Fitting"))
  (make-instance [TBM2] of BenchmarkScenario (type TBM)  (type-id 2) (description "Plate Drilling"))
  (make-instance [TBM3] of BenchmarkScenario (type TBM)  (type-id 3) (description "Fill a Box with Parts for Manual Assembly"))
  (make-instance [FBM1] of BenchmarkScenario (type FBM)  (type-id 1) (description "Object Perception Functionality"))
  (make-instance [FBM2] of BenchmarkScenario (type FBM)  (type-id 2) (description "Visual Servoing Functionality"))

  (make-instance [init-state] of InitState)

  (make-instance [benchmark] of Benchmark
    (current-scenario [NONE])
    (requested-scenario [NONE])
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
