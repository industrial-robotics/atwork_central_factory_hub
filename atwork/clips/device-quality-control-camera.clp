;---------------------------------------------------------------------------
;  device-quality-control-camera.clp - RoCKIn RefBox CLIPS - QCC interface
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defrule quality-control-camera-object-detected
  ?o <- (quality-control-camera-object ?object)
  =>
  (if (debug 3) then (printout t "Quality control camera found object " ?object crlf))

  (retract ?o) ; Remove the fact after the rule finishes

  (switch ?object
    (case FAULTY_PLATE then (bind ?object-id [ax-06]))
    (case PERFECT_PLATE then (bind ?object-id [ax-07]))
    (case UNUSABLE_PLATE then (bind ?object-id [ax-08]))
    (default (return))
  )

  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id ?object-id) (location-id [conveyor_belt-01]) (quantity 1))
  )
)