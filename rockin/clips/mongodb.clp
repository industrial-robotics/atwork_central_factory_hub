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

(defrule mongodb-fbm-start
  (declare (salience ?*PRIORITY_HIGH*))
  (benchmark-phase (id ?phase) (type FBM))
  (benchmark-state (phase-id ?phase) (state RUNNING) (prev-state PAUSED) (run ?run))
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

(defrule mongodb-fbm1-feedback
  (declare (salience ?*PRIORITY_HIGH*))
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  (benchmark-state (phase-id ?phase) (state RUNNING) (run ?run))
  ?bf <- (benchmark-feedback (time $?time) (type ?type)
      (object-class-name ?class) (object-instance-name ?instance)
      (object-pose-position-x ?object-pose-position-x)
      (object-pose-position-y ?object-pose-position-y)
      (object-pose-position-z ?object-pose-position-z)
      (object-pose-orientation-w ?object-pose-orientation-w)
      (object-pose-orientation-x ?object-pose-orientation-x)
      (object-pose-orientation-y ?object-pose-orientation-y)
      (object-pose-orientation-z ?object-pose-orientation-z))
  =>
  (retract ?bf)

  (bind ?bf (bson-create))
  (bson-append-time ?bf "timestamp" ?time)
  (bson-append ?bf "run_counter" ?run)
  (bson-append ?bf "action" ?type)

  (if (eq ?type RECOGNIZED)
   then
    (bson-append ?bf "class" ?class)
    (bson-append ?bf "object_id" ?instance)
    (bson-append ?bf "object_position_x" ?object-pose-position-x)
    (bson-append ?bf "object_position_y" ?object-pose-position-y)
    (bson-append ?bf "object_position_z" ?object-pose-position-z)
    (bson-append ?bf "object_orientation_w" ?object-pose-orientation-w)
    (bson-append ?bf "object_orientation_x" ?object-pose-orientation-x)
    (bson-append ?bf "object_orientation_y" ?object-pose-orientation-y)
    (bson-append ?bf "object_orientation_z" ?object-pose-orientation-z)
  )

  (mongodb-insert "llsfrb.benchmark" ?bf)
  (bson-destroy ?bf)
)

(defrule mongodb-fbm2-feedback-peer
  (declare (salience ?*PRIORITY_HIGH*))
  (benchmark-phase (id ?phase) (type FBM) (type-id 2))
  (benchmark-state (phase-id ?phase) (state RUNNING) (run ?run))
  ?bf <- (benchmark-feedback (source PEER) (time $?time) (type ?type)
      (grasp-notification ?notification)
      (object-class-name ?class) (object-instance-name ?instance)
      (end-effector-pose-position-x ?end-effector-pose-position-x)
      (end-effector-pose-position-y ?end-effector-pose-position-y)
      (end-effector-pose-position-z ?end-effector-pose-position-z)
      (end-effector-pose-orientation-w ?end-effector-pose-orientation-w)
      (end-effector-pose-orientation-x ?end-effector-pose-orientation-x)
      (end-effector-pose-orientation-y ?end-effector-pose-orientation-y)
      (end-effector-pose-orientation-z ?end-effector-pose-orientation-z))
  =>
  (retract ?bf)

  (bind ?bf (bson-create))
  (bson-append-time ?bf "timestamp" ?time)
  (bson-append ?bf "run_counter" ?run)
  (bson-append ?bf "action" ?type)

  (if (eq ?type LIFTED)
   then
    (bson-append ?bf "object_id" ?instance)
    (bson-append ?bf "class" ?class)
    (bson-append ?bf "end_effector_position_x" ?end-effector-pose-position-x)
    (bson-append ?bf "end_effector_position_y" ?end-effector-pose-position-y)
    (bson-append ?bf "end_effector_position_z" ?end-effector-pose-position-z)
    (bson-append ?bf "end_effector_orientation_w" ?end-effector-pose-orientation-w)
    (bson-append ?bf "end_effector_orientation_x" ?end-effector-pose-orientation-x)
    (bson-append ?bf "end_effector_orientation_y" ?end-effector-pose-orientation-y)
    (bson-append ?bf "end_effector_orientation_z" ?end-effector-pose-orientation-z)
  )

  (mongodb-insert "llsfrb.benchmark" ?bf)
  (bson-destroy ?bf)
)

(defrule mongodb-fbm2-feedback-client
  (declare (salience ?*PRIORITY_HIGH*))
  (benchmark-phase (id ?phase) (type FBM) (type-id 2))
  (benchmark-state (phase-id ?phase) (state PAUSED|FINISHED) (run ?run))
  ?bf <- (benchmark-feedback (source CLIENT) (time $?time) (type ?type))
  =>
  (retract ?bf)

  (bind ?bf (bson-create))
  (bson-append-time ?bf "timestamp" ?time)
  (bson-append ?bf "run_counter" ?run)
  (bson-append ?bf "action" ?type)

  (mongodb-insert "llsfrb.benchmark" ?bf)
  (bson-destroy ?bf)
)
