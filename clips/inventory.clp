;---------------------------------------------------------------------------
;  facts.clp - RoCKIn RefBox CLIPS - inventory specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass Inventory (is-a USER) (role concrete)
  (multislot items (type INSTANCE) (allowed-classes Item))
)

(defmessage-handler Inventory create-msg ()
  "Create a ProtoBuf message for the inventory"

  ; Instantiate a new inventory
  (bind ?pb-inventory (pb-create "rockin_msgs.Inventory"))

  (foreach ?item ?self:items
    (pb-add-list ?pb-inventory "items" (send ?item create-msg))
  )

  (return ?pb-inventory)
)

(defrule init-inventory
  (init)
  =>
  (make-instance [inventory] of Inventory)
)