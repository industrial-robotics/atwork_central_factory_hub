;---------------------------------------------------------------------------
;  order-info.clp - RoCKIn RefBox CLIPS - order info specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass OrderInfo (is-a USER) (role concrete)
  ; The order info represents all currently existing orders in the system
)

(defmessage-handler OrderInfo create-msg ()
  "Create a ProtoBuf message of an order info"

  (bind ?oi (pb-create "rockin_msgs.OrderInfo"))

  (do-for-all-facts ((?order order)) TRUE
    (bind ?o (net-create-Order ?order))
    (pb-add-list ?oi "orders" ?o) ; destroys ?o
  )

  (return ?oi)
)


(defrule init-order-info
  (init)
  =>
  (make-instance [order-info] of OrderInfo)
)