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

  (bind ?pb-benchmark-scenario (pb-create "rockin_msgs.BenchmarkScenario"))
 
  (pb-set-field ?pb-benchmark-scenario "type" ?self:type)
  (pb-set-field ?pb-benchmark-scenario "type_id" ?self:type-id)
  (pb-set-field ?pb-benchmark-scenario "description" ?self:description)

  (return ?pb-benchmark-scenario)
)

(defmessage-handler BenchmarkScenario setup (?time ?state-machine)
)

(defmessage-handler BenchmarkScenario handle-feedback (?pb-msg ?time ?name ?team)
  (return CONTINUE)
)


(defclass NoneBenchmarkScenario (is-a BenchmarkScenario) (role concrete))

(defmessage-handler NoneBenchmarkScenario setup (?time ?state-machine)
  (make-instance [init-state] of InitState)

  (make-instance ?state-machine of StateMachine
    (current-state [init-state])
    (states [init-state])
  )
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
  (send ?self put-current-scenario ?self:requested-scenario)


  ; Remove all items from the inventory
  (do-for-all-instances ((?inventory Inventory))
    (foreach ?item (send ?inventory get-items)
      (unmake-instance ?item)
    )
    (slot-delete$ ?inventory items 1 (length$ (send ?inventory get-items)))
  )

  ; Remove all orders from the order info
  (do-for-all-instances ((?order-info OrderInfo))
    (foreach ?order (send ?order-info get-orders)
      (unmake-instance ?order)
    )
    (slot-delete$ ?order-info orders 1 (length$ (send ?order-info get-orders)))
  )


  ; TODO: Remove all states and their transitions


  (send ?self:current-scenario setup ?self:time ?self:state-machine)
)

(defmessage-handler Benchmark request-scenario (?type ?type-id)
  (foreach ?scenario (send ?self get-registered-scenarios)
    (bind ?scenario-type (send ?scenario get-type))
    (bind ?scenario-type-id (send ?scenario get-type-id))

    (if (and (eq ?type ?scenario-type) (eq ?type-id ?scenario-type-id))
     then
      (send ?self put-requested-scenario ?scenario)
      (return)
    )
  )

  (printout t "Requested benchmark scenario " ?type ?type-id " does not exist" crlf)
)

(defmessage-handler Benchmark handle-feedback (?pb-msg ?time ?name ?team)
  (bind ?scenario (send ?self get-current-scenario))
  (return (send ?scenario handle-feedback ?pb-msg ?time ?name ?team))
)


(defrule init-benchmark
  (init)
  =>
  (make-instance [NONE] of NoneBenchmarkScenario (type NONE) (type-id 0) (description "No benchmark running"))

  (make-instance [benchmark] of Benchmark
    (time (make-instance of BenchmarkTime))
    (registered-scenarios [NONE])
  )

  (send [benchmark] request-scenario NONE 0)
  (send [benchmark] switch-scenario)
)

(defrule benchmark-update
  (time $?now)
  ?bm <- (object (is-a Benchmark))
  =>
  (bind ?state-machine (send ?bm get-state-machine))
  (send ?state-machine update)
)