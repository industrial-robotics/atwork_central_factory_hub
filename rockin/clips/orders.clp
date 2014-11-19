;---------------------------------------------------------------------------
;  orders.clp - RoCKIn RefBox CLIPS order processing
;
;  Created: Sun Feb 24 19:40:32 2013
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

; Sort orders by ID, such that do-for-all-facts on the orders deftemplate
; iterates in a nice order, e.g. for net-send-OrderIntstruction
(defrule sort-orders
  (declare (salience ?*PRIORITY_HIGH*))
  ?oa <- (order (id ?id-a))
  ?ob <- (order (id ?id-b&:(> ?id-a ?id-b)&:(< (fact-index ?oa) (fact-index ?ob))))
  =>
  (modify ?oa)
)
