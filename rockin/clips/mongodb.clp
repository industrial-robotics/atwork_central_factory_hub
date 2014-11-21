;---------------------------------------------------------------------------
;  mongodb.clp - RoCKIn RefBox CLIPS MongoDB logging
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defrule mongodb-net-client-connected
  (declare (salience ?*PRIORITY_HIGH*))
  (protobuf-server-client-connected ?client-id ?host ?port)
  =>
  (bind ?client-doc (bson-create))
  (bson-append-time ?client-doc "session" ?*START-TIME*)
  (bson-append-time ?client-doc "connect-time" (now))
  (bson-append ?client-doc "client-id" ?client-id)
  (bson-append ?client-doc "host" ?host)
  (bson-append ?client-doc "port" ?port)
  (mongodb-insert "llsfrb.clients" ?client-doc)
  (bson-destroy ?client-doc)
)

(defrule mongodb-net-client-disconnected
  (declare (salience ?*PRIORITY_HIGH*))
  (protobuf-server-client-disconnected ?client-id)
  =>
  (bind ?client-update-doc (bson-create))
  (bson-append-time ?client-update-doc "disconnect-time" (now))

  (bind ?update-query (bson-create))
  (bson-append-time ?update-query "session" ?*START-TIME*)
  (bson-append ?update-query "client-id" ?client-id)

  (mongodb-update "llsfrb.clients" ?client-update-doc ?update-query)
  (bson-destroy ?client-update-doc)
  (bson-destroy ?update-query)
)

(defrule mongodb-fbm-start-or-continue
  (declare (salience ?*PRIORITY_HIGH*))
  (benchmark-phase (id ?phase) (type FBM))
  (benchmark-state (phase-id ?phase) (state RUNNING) (prev-state INIT|PAUSED) (run ?run))
  (selected-object (object-id ?id))
  =>
  (bind ?bf (bson-create))
  (bson-append-time ?bf "timestamp" (now))
  (bson-append ?bf "run_counter" ?run)

  (bson-append ?bf "action" "STARTED")
  (do-for-fact ((?oi object-identifier)) (eq ?oi:id ?id)
    (bson-append ?bf "object_id" ?oi:description)
  )

  (mongodb-insert "llsfrb.benchmark" ?bf)
  (bson-destroy ?bf)
)

(defrule mongodb-fbm-run-timeout
  (declare (salience ?*PRIORITY_HIGH*))
  (benchmark-phase (id ?phase) (type FBM))
  (benchmark-state (phase-id ?phase) (state RUNNING) (run ?run)
      (max-time ?max-time) (benchmark-time ?benchmark-time&:(>= ?benchmark-time ?max-time)))
  =>
  (bind ?bf (bson-create))
  (bson-append-time ?bf "timestamp" (now))
  (bson-append ?bf "run_counter" ?run)

  (bson-append ?bf "action" "TIMEOUT")

  (mongodb-insert "llsfrb.benchmark" ?bf)
  (bson-destroy ?bf)
)

