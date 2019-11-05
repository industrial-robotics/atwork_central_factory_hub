;---------------------------------------------------------------------------
;  location.clp - RoCKIn RefBox CLIPS - location specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass LocationIdentifier (is-a USER) (role concrete)
  ; a location id, such as Shelf 1 consists of *type* and *instance-id*
  (slot type (type SYMBOL) (allowed-values SH WS WP CB ROBOT))
  (slot instance-id (type INTEGER))

  (multislot description (type STRING) (cardinality 0 1))
)

(defmessage-handler LocationIdentifier create-msg ()
  "Create a ProtoBuf message for a location identifier"

  ; Instantiate a new item for the protobuf message
  (bind ?pb-location-identifier (pb-create "atwork_pb_msgs.LocationIdentifier"))

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
  (make-instance shelf-01 of LocationIdentifier (type SH) (instance-id  1) (description "Shelf 1"))
  (make-instance shelf-02 of LocationIdentifier (type SH) (instance-id  2) (description "Shelf 2"))

  ; Workstations
  (make-instance workstation-01 of LocationIdentifier (type WS) (instance-id 1) (description "Workstation 1"))
  (make-instance workstation-02 of LocationIdentifier (type WS) (instance-id 2) (description "Workstation 2"))
  (make-instance workstation-03 of LocationIdentifier (type WS) (instance-id 3) (description "Workstation 3"))
  (make-instance workstation-04 of LocationIdentifier (type WS) (instance-id 4) (description "Workstation 4"))
  (make-instance workstation-05 of LocationIdentifier (type WS) (instance-id 5) (description "Workstation 5"))
  (make-instance workstation-06 of LocationIdentifier (type WS) (instance-id 6) (description "Workstation 6"))
  (make-instance workstation-07 of LocationIdentifier (type WS) (instance-id 7) (description "Workstation 7"))
  (make-instance workstation-08 of LocationIdentifier (type WS) (instance-id 8) (description "Workstation 8"))
  (make-instance workstation-09 of LocationIdentifier (type WS) (instance-id 9) (description "Workstation 9"))
  (make-instance workstation-10 of LocationIdentifier (type WS) (instance-id 10) (description "Workstation 10"))
  (make-instance workstation-11 of LocationIdentifier (type WS) (instance-id 11) (description "Workstation 11"))
  (make-instance workstation-12 of LocationIdentifier (type WS) (instance-id 12) (description "Workstation 12"))
  (make-instance workstation-14 of LocationIdentifier (type WS) (instance-id 14) (description "Workstation 14"))
 (make-instance workstation-13 of LocationIdentifier (type WS) (instance-id 13) (description "Workstation 13"))
  (make-instance workstation-15 of LocationIdentifier (type WS) (instance-id 15) (description "Workstation 15"))
  (make-instance workstation-16 of LocationIdentifier (type WS) (instance-id 16) (description "Workstation 16"))

  ; Waypoints
  (make-instance waypoint-01 of LocationIdentifier (type WP) (instance-id 1) (description "Waypoint 1"))
  (make-instance waypoint-02 of LocationIdentifier (type WP) (instance-id 2) (description "Waypoint 2"))
  (make-instance waypoint-03 of LocationIdentifier (type WP) (instance-id 3) (description "Waypoint 3"))
  (make-instance waypoint-04 of LocationIdentifier (type WP) (instance-id 4) (description "Waypoint 4"))
  (make-instance waypoint-05 of LocationIdentifier (type WP) (instance-id 5) (description "Waypoint 5"))
  (make-instance waypoint-06 of LocationIdentifier (type WP) (instance-id 6) (description "Waypoint 6"))
  (make-instance waypoint-07 of LocationIdentifier (type WP) (instance-id 7) (description "Waypoint 7"))
  (make-instance waypoint-08 of LocationIdentifier (type WP) (instance-id 8) (description "Waypoint 8"))
  (make-instance waypoint-09 of LocationIdentifier (type WP) (instance-id 9) (description "Waypoint 9"))
  (make-instance waypoint-10 of LocationIdentifier (type WP) (instance-id 10) (description "Waypoint 10"))
  (make-instance waypoint-11 of LocationIdentifier (type WP) (instance-id 11) (description "Waypoint 11"))
  (make-instance waypoint-12 of LocationIdentifier (type WP) (instance-id 12) (description "Waypoint 12"))
  (make-instance waypoint-13 of LocationIdentifier (type WP) (instance-id 13) (description "Waypoint 13"))
  (make-instance waypoint-14 of LocationIdentifier (type WP) (instance-id 14) (description "Waypoint 14"))
  (make-instance waypoint-15 of LocationIdentifier (type WP) (instance-id 15) (description "Waypoint 15"))

  ; Conveyor belt
  (make-instance conveyorbelt-01 of LocationIdentifier (type CB) (instance-id 1) (description "Conveyor Belt"))
  (make-instance conveyorbelt-02 of LocationIdentifier (type CB) (instance-id 2) (description "Rotating Table"))

  ; Precision Placement Platform
  (make-instance precision-01 of LocationIdentifier (type PP) (instance-id 1) (description "Precision Platform"))

  ; Robot
  (make-instance robot of LocationIdentifier (type ROBOT) (instance-id 1) (description "Robot"))
)
