;---------------------------------------------------------------------------
;  facts.clp - RoCKIn RefBox CLIPS - inventory specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass Inventory (is-a USER) (role concrete))

(defmessage-handler Inventory create-msg ()
  "Create a ProtoBuf message for the inventory"

  ; Instantiate a new inventory
  (bind ?pb-inventory (pb-create "rockin_msgs.Inventory"))

  ; Iterate over all asserted items (which form the inventory)
  (do-for-all-facts ((?item item)) TRUE
    ; Instantiate a new item for the protobuf message
    (bind ?pb-item (pb-create "rockin_msgs.Inventory.Item"))

    (pb-set-field ?pb-item "object" (net-create-ObjectIdentifier ?item:object-id))

    ; Only set the location if is available
    (if (<> (length$ ?item:location-id) 0) then
      (pb-set-field ?pb-item "location" (net-create-LocationIdentifier (nth$ 1 ?item:location-id)))
    )

    ; Only set the container if it is available
    (if (<> (length$ ?item:container-id) 0) then
      (pb-set-field ?pb-item "container" (net-create-ObjectIdentifier (nth$ 1 ?item:container-id)))
    )

    ; Only set the quantity if it is available
    (if (<> (length$ ?item:quantity) 0) then
      (pb-set-field ?pb-item "quantity" (nth$ 1 ?item:quantity))
    )

    (pb-add-list ?pb-inventory "items" ?pb-item)
  )

  (return ?pb-inventory)
)

(defrule init-inventory
  (init)
  =>
  (make-instance [inventory] of Inventory)
)