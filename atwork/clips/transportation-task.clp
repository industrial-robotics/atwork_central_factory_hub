;---------------------------------------------------------------------------
;  transportation-task.clp - RoCKIn RefBox CLIPS - transportation-task specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass TransportationTask (is-a USER) (role concrete)
  (slot object-id (type INSTANCE) (allowed-classes ObjectIdentifier))
  (multislot container-id (type INSTANCE) (allowed-classes ObjectIdentifier) (cardinality 0 1))
  (slot quantity-delivered (type INTEGER) (default 0))
  (multislot quantity-requested (type INTEGER) (cardinality 0 1))
  (multislot destination-id (type INSTANCE) (allowed-classes LocationIdentifier) (cardinality 0 1))
  (multislot source-id (type INSTANCE) (allowed-classes LocationIdentifier) (cardinality 0 1))
  (multislot processing-team (type STRING) (cardinality 0 1))
)

(defmessage-handler TransportationTask create-msg ()
  "Create a ProtoBuf message of an transportation-task"

  (bind ?o (pb-create "atwork_pb_msgs.TransportationTask"))

  (bind ?oi (send ?self:object-id create-msg))
  (pb-set-field ?o "object" ?oi)  ; destroys ?oi

  (if (<> (length$ ?self:container-id) 0) then
    (bind ?ci (send (nth$ 1 ?self:container-id) create-msg))
    (pb-set-field ?o "container" ?ci)  ; destroys ?ci
  )

  (pb-set-field ?o "quantity_delivered" ?self:quantity-delivered)

  (if (<> (length$ ?self:quantity-requested) 0) then
    (pb-set-field ?o "quantity_requested" (nth$ 1 ?self:quantity-requested))
  )

  (if (<> (length$ ?self:destination-id) 0) then
    (bind ?li (send (nth$ 1 ?self:destination-id) create-msg))
    (pb-set-field ?o "destination" ?li) ; destroys ?li
  )

  (if (<> (length$ ?self:source-id) 0) then
    (bind ?si (send (nth$ 1 ?self:source-id) create-msg))
    (pb-set-field ?o "source" ?si) ; destroys ?si
  )

  (if (<> (length$ ?self:processing-team) 0) then
    (pb-set-field ?o "processing_team" (nth$ 1 ?self:processing-team))
  )

  (return ?o)
)

