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

(defrule mongodb-net-recv-BenchmarkFeedback-fbm1-peer
  (declare (salience ?*PRIORITY_HIGH*))
  ?mf <- (protobuf-msg (type "rockin_msgs.BenchmarkFeedback") (ptr ?p)
          (rcvd-at $?rcvd-at) (rcvd-from ?from-host ?from-port) (rcvd-via BROADCAST))
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  (benchmark-state (phase-id ?phase) (state RUNNING) (run ?run))
  =>
  (if (and
       (pb-has-field ?p "object_class_name")
       (pb-has-field ?p "object_instance_name")
       (pb-has-field ?p "object_pose")
      )
   then
    (printout t "Benchmarking message valid" crlf)

    (bind ?bf (bson-create))
    (bson-append-time ?bf "timestamp" (now))
    (bson-append ?bf "run_counter" ?run)
    (bson-append ?bf "action" "RECOGNIZED")
    (bson-append ?bf "class" (pb-field-value ?p "object_class_name"))
    (bson-append ?bf "object_id" (pb-field-value ?p "object_instance_name"))

    (bind ?pose (pb-field-value ?p "object_pose"))
    (bind ?position (pb-field-value ?pose "position"))
    (bind ?orientation (pb-field-value ?pose "orientation"))

    (bson-append ?bf "position_x" (pb-field-value ?position "x"))
    (bson-append ?bf "position_y" (pb-field-value ?position "y"))
    (bson-append ?bf "position_z" (pb-field-value ?position "z"))
    (bson-append ?bf "orientation_x" (pb-field-value ?orientation "x"))
    (bson-append ?bf "orientation_y" (pb-field-value ?orientation "y"))
    (bson-append ?bf "orientation_z" (pb-field-value ?orientation "z"))
    (bson-append ?bf "orientation_w" (pb-field-value ?orientation "w"))

    (mongodb-insert "llsfrb.benchmark" ?bf)
    (bson-destroy ?bf)
   else
    (printout t "Benchmarking message invalid" crlf)

    (bind ?bf (bson-create))
    (bson-append-time ?bf "timestamp" (now))
    (bson-append ?bf "run_counter" ?run)
    (bson-append ?bf "action" "TIMEOUT")

    (mongodb-insert "llsfrb.benchmark" ?bf)
    (bson-destroy ?bf)
  )
)

