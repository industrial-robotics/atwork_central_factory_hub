;---------------------------------------------------------------------------
;  location.clp - RoCKIn RefBox CLIPS - location specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass LocationIdentifier (is-a USER) (role concrete)
  ; a location id, such as SHELF-01 consists of *type* and *instance-id*
  (slot type (type SYMBOL) (allowed-values SH WS CB ROBOT))
  (slot instance-id (type INTEGER))

  (multislot description (type STRING) (cardinality 0 1))
)

(defmessage-handler LocationIdentifier create-msg ()
  "Create a ProtoBuf message for a location identifier"

  ; Instantiate a new item for the protobuf message
  (bind ?pb-location-identifier (pb-create "rockin_msgs.LocationIdentifier"))

  (pb-set-field ?pb-location-identifier "type" ?self:type)
  (pb-set-field ?pb-location-identifier "instance_id" ?self:instance-id)

  ; Only set the description if it is available
  (if (<> (length$ ?self:description) 0) then
    (pb-set-field ?pb-location-identifier "description" ?self:description)
  )

  (return ?pb-location-identifier)
)


(defrule init-locations
  (init)
  =>
  ; Shelves
  (make-instance shelf-01 of LocationIdentifier (type SH) (instance-id  1) (description "SHELF-01"))
  (make-instance shelf-02 of LocationIdentifier (type SH) (instance-id  2) (description "SHELF-02"))
  (make-instance shelf-03 of LocationIdentifier (type SH) (instance-id  3) (description "SHELF-03"))
  (make-instance shelf-04 of LocationIdentifier (type SH) (instance-id  4) (description "SHELF-04"))
  (make-instance shelf-05 of LocationIdentifier (type SH) (instance-id  5) (description "SHELF-05"))
  (make-instance shelf-06 of LocationIdentifier (type SH) (instance-id  6) (description "SHELF-06"))
  (make-instance shelf-07 of LocationIdentifier (type SH) (instance-id  7) (description "SHELF-07"))
  (make-instance shelf-08 of LocationIdentifier (type SH) (instance-id  8) (description "SHELF-08"))
  (make-instance shelf-09 of LocationIdentifier (type SH) (instance-id  9) (description "SHELF-09"))
  (make-instance shelf-10 of LocationIdentifier (type SH) (instance-id 10) (description "SHELF-10"))
  (make-instance shelf-11 of LocationIdentifier (type SH) (instance-id 11) (description "SHELF-11"))
  (make-instance shelf-12 of LocationIdentifier (type SH) (instance-id 12) (description "SHELF-12"))
  (make-instance shelf-13 of LocationIdentifier (type SH) (instance-id 13) (description "SHELF-13"))
  (make-instance shelf-14 of LocationIdentifier (type SH) (instance-id 14) (description "SHELF-14"))
  (make-instance shelf-15 of LocationIdentifier (type SH) (instance-id 15) (description "SHELF-15"))
  (make-instance shelf-16 of LocationIdentifier (type SH) (instance-id 16) (description "SHELF-16"))
  (make-instance shelf-17 of LocationIdentifier (type SH) (instance-id 17) (description "SHELF-17"))
  (make-instance shelf-18 of LocationIdentifier (type SH) (instance-id 18) (description "SHELF-18"))
  (make-instance shelf-19 of LocationIdentifier (type SH) (instance-id 19) (description "SHELF-19"))
  (make-instance shelf-20 of LocationIdentifier (type SH) (instance-id 20) (description "SHELF-20"))
  (make-instance shelf-21 of LocationIdentifier (type SH) (instance-id 21) (description "SHELF-21"))
  (make-instance shelf-22 of LocationIdentifier (type SH) (instance-id 22) (description "SHELF-22"))
  (make-instance shelf-23 of LocationIdentifier (type SH) (instance-id 23) (description "SHELF-23"))
  (make-instance shelf-24 of LocationIdentifier (type SH) (instance-id 24) (description "SHELF-24"))

  ; Workstations
  (make-instance workstation-01 of LocationIdentifier (type WS) (instance-id 1) (description "WORKSTATION-01"))
  (make-instance workstation-02 of LocationIdentifier (type WS) (instance-id 2) (description "WORKSTATION-02"))
  (make-instance workstation-03 of LocationIdentifier (type WS) (instance-id 3) (description "WORKSTATION-03"))
  (make-instance workstation-04 of LocationIdentifier (type WS) (instance-id 4) (description "WORKSTATION-04"))
  (make-instance workstation-05 of LocationIdentifier (type WS) (instance-id 5) (description "WORKSTATION-05"))
  (make-instance workstation-06 of LocationIdentifier (type WS) (instance-id 6) (description "WORKSTATION-06"))
  (make-instance workstation-07 of LocationIdentifier (type WS) (instance-id 7) (description "WORKSTATION-07"))

  ; Conveyor belt
  (make-instance conveyor_belt-01 of LocationIdentifier (type CB) (instance-id 1) (description "CONVEYOR_BELT-01"))

  ; Robot
  (make-instance robot of LocationIdentifier (type ROBOT) (instance-id 1) (description "ROBOT"))
)