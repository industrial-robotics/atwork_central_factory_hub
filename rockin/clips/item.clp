;---------------------------------------------------------------------------
;  facts.clp - RoCKIn RefBox CLIPS - item specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass Item (is-a USER) (role concrete)
  (slot object-id (type INSTANCE) (allowed-classes ObjectIdentifier))
  (multislot container-id (type INSTANCE) (allowed-classes ObjectIdentifier) (cardinality 0 1))
  (multislot location-id (type INSTANCE) (allowed-classes LocationIdentifier) (cardinality 0 1))
  (multislot quantity (type INTEGER) (cardinality 0 1))
)

(defmessage-handler Item create-msg ()
  "Create a ProtoBuf message of an item"

  ; Instantiate a new item for the protobuf message
  (bind ?pb-item (pb-create "rockin_msgs.Inventory.Item"))

  (pb-set-field ?pb-item "object" (send ?self:object-id create-msg))

  ; Only set the location if it is available
  (if (<> (length$ ?self:location-id) 0) then
    (bind ?location (nth$ 1 ?self:location-id))
    (pb-set-field ?pb-item "location" (send ?location create-msg))
  )

  ; Only set the container if it is available
  (if (<> (length$ ?self:container-id) 0) then
    (bind ?container (nth$ 1 ?self:container-id))
    (pb-set-field ?pb-item "container" (send ?container create-msg))
  )

  ; Only set the quantity if it is available
  (if (<> (length$ ?self:quantity) 0) then
    (pb-set-field ?pb-item "quantity" (nth$ 1 ?self:quantity))
  )

  (return ?pb-item)
)
