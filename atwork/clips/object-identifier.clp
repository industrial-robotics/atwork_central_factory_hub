;---------------------------------------------------------------------------
;  object-identifier.clp - RoCKIn RefBox CLIPS - object identifier specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass ObjectIdentifier (is-a USER) (role concrete)
  ; the object id (see rulebook), such as AX-01 consists of *type* and *type-id*
  (slot type (type SYMBOL) (allowed-symbols
    F20_20_B     ; // Small Aluminium Profile (Black)
    F20_20_G     ; // Small Aluminium Profile (Grey)
    S40_40_B     ; // Big Aluminium Profile (Black)
    S40_40_G     ; // Big Aluminium Profile (Grey)
    M20_100      ; // Screw (Bolt)
    M20          ; // Small Nut
    M30          ; // Large Nut
    R20          ; // Plastic Tube
    BEARING_BOX  ; //
    BEARING      ; //
    AXIS         ; //
    DISTANCE_TUBE; //
    MOTOR        ; //
    CONTAINER_B  ; // A Container (Blue)
    CONTAINER_R  ; // A Container (Red)
  ))
  (slot type-id (type INTEGER))

  (multislot instance-id (type INTEGER) (cardinality 0 1))
  (multislot description (type STRING) (cardinality 0 1))
)

(defmessage-handler ObjectIdentifier create-msg ()
  "Create a ProtoBuf message for an object identifier"

  (bind ?pb-object-identifier (pb-create "atwork_pb_msgs.ObjectIdentifier"))

  (pb-set-field ?pb-object-identifier "type" ?self:type)
  (pb-set-field ?pb-object-identifier "type_id" ?self:type-id)

  ; Only set the instance id if it is available
  (if (<> (length$ ?self:instance-id) 0) then
    (pb-set-field ?pb-object-identifier "instance_id" ?self:instance-id)
  )

  ; Only set the description if it is available
  (if (<> (length$ ?self:description) 0) then
    (pb-set-field ?pb-object-identifier "description" ?self:description)
  )

  (return ?pb-object-identifier)
)


(defrule init-object-identifiers
  (init)
  =>
  (make-instance [F20_20_B]      of ObjectIdentifier (type F20_20_B)      (type-id 1) (description "Small Black Alu. Profile"))
  (make-instance [F20_20_G]      of ObjectIdentifier (type F20_20_G)      (type-id 1) (description "Small Grey Alu. Profile"))
  (make-instance [S40_40_B]      of ObjectIdentifier (type S40_40_B)      (type-id 1) (description "Large Black Alu. Profile"))
  (make-instance [S40_40_G]      of ObjectIdentifier (type S40_40_G)      (type-id 1) (description "Large Grey Alu. Profile"))
  (make-instance [M20_100]       of ObjectIdentifier (type M20_100)       (type-id 1) (description "Bolt"))
  (make-instance [M20]           of ObjectIdentifier (type M20)           (type-id 1) (description "Small Nut"))
  (make-instance [M30]           of ObjectIdentifier (type M30)           (type-id 1) (description "Large Nut"))
  (make-instance [R20]           of ObjectIdentifier (type R20)           (type-id 1) (description "Plastic Tube"))
  (make-instance [BEARING_BOX]   of ObjectIdentifier (type BEARING_BOX)   (type-id 1) (description "Bearing Box"))
  (make-instance [BEARING]       of ObjectIdentifier (type BEARING)       (type-id 1) (description "Bearing"))
  (make-instance [AXIS]          of ObjectIdentifier (type AXIS)          (type-id 1) (description "Axis"))
  (make-instance [DISTANCE_TUBE] of ObjectIdentifier (type DISTANCE_TUBE) (type-id 1) (description "Distance Tube"))
  (make-instance [MOTOR]         of ObjectIdentifier (type MOTOR)         (type-id 1) (description "Motor"))
  (make-instance [CONTAINER_B]   of ObjectIdentifier (type CONTAINER_B)   (type-id 1) (description "Blue Container"))
  (make-instance [CONTAINER_R]   of ObjectIdentifier (type CONTAINER_R)   (type-id 1) (description "Red Container"))
)
