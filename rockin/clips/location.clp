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


(definstances locations
  ; Shelves
  (shelf-01 of LocationIdentifier (id  1) (type SH) (instance-id  1) (description "SHELF-01"))
  (shelf-02 of LocationIdentifier (id  2) (type SH) (instance-id  2) (description "SHELF-02"))
  (shelf-03 of LocationIdentifier (id  3) (type SH) (instance-id  3) (description "SHELF-03"))
  (shelf-04 of LocationIdentifier (id  4) (type SH) (instance-id  4) (description "SHELF-04"))
  (shelf-05 of LocationIdentifier (id  5) (type SH) (instance-id  5) (description "SHELF-05"))
  (shelf-06 of LocationIdentifier (id  6) (type SH) (instance-id  6) (description "SHELF-06"))
  (shelf-07 of LocationIdentifier (id  7) (type SH) (instance-id  7) (description "SHELF-07"))
  (shelf-08 of LocationIdentifier (id  8) (type SH) (instance-id  8) (description "SHELF-08"))
  (shelf-09 of LocationIdentifier (id  9) (type SH) (instance-id  9) (description "SHELF-09"))
  (shelf-10 of LocationIdentifier (id 10) (type SH) (instance-id 10) (description "SHELF-10"))
  (shelf-11 of LocationIdentifier (id 11) (type SH) (instance-id 11) (description "SHELF-11"))
  (shelf-12 of LocationIdentifier (id 12) (type SH) (instance-id 12) (description "SHELF-12"))
  (shelf-13 of LocationIdentifier (id 13) (type SH) (instance-id 13) (description "SHELF-13"))
  (shelf-14 of LocationIdentifier (id 14) (type SH) (instance-id 14) (description "SHELF-14"))
  (shelf-15 of LocationIdentifier (id 15) (type SH) (instance-id 15) (description "SHELF-15"))
  (shelf-16 of LocationIdentifier (id 16) (type SH) (instance-id 16) (description "SHELF-16"))
  (shelf-17 of LocationIdentifier (id 17) (type SH) (instance-id 17) (description "SHELF-17"))
  (shelf-18 of LocationIdentifier (id 18) (type SH) (instance-id 18) (description "SHELF-18"))
  (shelf-19 of LocationIdentifier (id 19) (type SH) (instance-id 19) (description "SHELF-19"))
  (shelf-20 of LocationIdentifier (id 20) (type SH) (instance-id 20) (description "SHELF-20"))
  (shelf-21 of LocationIdentifier (id 21) (type SH) (instance-id 21) (description "SHELF-21"))
  (shelf-22 of LocationIdentifier (id 22) (type SH) (instance-id 22) (description "SHELF-22"))
  (shelf-23 of LocationIdentifier (id 23) (type SH) (instance-id 23) (description "SHELF-23"))
  (shelf-24 of LocationIdentifier (id 24) (type SH) (instance-id 24) (description "SHELF-24"))

  ; Workstations
  (workstation-01 of LocationIdentifier (id 30) (type WS) (instance-id 1) (description "WORKSTATION-01"))
  (workstation-02 of LocationIdentifier (id 31) (type WS) (instance-id 2) (description "WORKSTATION-02"))
  (workstation-03 of LocationIdentifier (id 32) (type WS) (instance-id 3) (description "WORKSTATION-03"))
  (workstation-04 of LocationIdentifier (id 33) (type WS) (instance-id 4) (description "WORKSTATION-04"))
  (workstation-05 of LocationIdentifier (id 34) (type WS) (instance-id 5) (description "WORKSTATION-05"))
  (workstation-06 of LocationIdentifier (id 35) (type WS) (instance-id 6) (description "WORKSTATION-06"))
  (workstation-07 of LocationIdentifier (id 36) (type WS) (instance-id 7) (description "WORKSTATION-07"))

  ; Conveyor belt
  (conveyor_belt-01 of LocationIdentifier (id 40) (type CB) (instance-id 1) (description "CONVEYOR_BELT-01"))

  ; Robot
  (robot of LocationIdentifier (id 50) (type ROBOT) (instance-id 1) (description "ROBOT"))
)