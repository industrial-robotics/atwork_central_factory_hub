;---------------------------------------------------------------------------
;  order.clp - RoCKIn RefBox CLIPS - order specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass Order (is-a USER) (role concrete)
  (slot next-id (type INTEGER) (storage shared) (default 0))

  (slot id (type INTEGER))
  (slot status (type SYMBOL) (allowed-values OFFERED TIMEOUT IN_PROGRESS PAUSED ABORTED FINISHED))
  (slot object-id (type INSTANCE) (allowed-classes ObjectIdentifier))
  (multislot container-id (type INSTANCE) (allowed-classes ObjectIdentifier) (cardinality 0 1))
  (slot quantity-delivered (type INTEGER) (default 0))
  (multislot quantity-requested (type INTEGER) (cardinality 0 1))
  (multislot destination-id (type INSTANCE) (allowed-classes LocationIdentifier) (cardinality 0 1))
  (multislot source-id (type INSTANCE) (allowed-classes LocationIdentifier) (cardinality 0 1))
  (multislot processing-team (type STRING) (cardinality 0 1))
)

(defmessage-handler Order init ()
  ; first, initialize the other slots
  (call-next-handler)

  ; then, overwrite the id slot from the class variable and increase the next id
  (modify-instance ?self (id ?self:next-id) (next-id (+ ?self:next-id 1)))
)

(defmessage-handler Order create-msg ()
  "Create a ProtoBuf message of an order"

  (bind ?o (pb-create "rockin_msgs.Order"))

  (pb-set-field ?o "id" ?self:id)
  (pb-set-field ?o "status" ?self:status)

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


; When all instances of a class are deleted, the shared slots are reset.
; Prevent this by creating a dummy order, which does not get deleted.
(defrule make-dummy-order
  (init)
  =>
  (make-instance [dummy] of Order)
)
