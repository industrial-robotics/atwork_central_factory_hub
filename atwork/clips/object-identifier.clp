;---------------------------------------------------------------------------
;  object-identifier.clp - RoCKIn RefBox CLIPS - object identifier specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass ObjectIdentifier (is-a USER) (role concrete)
  ; the object id (see rulebook), such as AX-01 consists of *type* and *type-id*
  (slot type (type SYMBOL) (allowed-values EM AX ER))
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
  ; Navigation Goal (class)
  (make-instance [NAV_GOAL] of ObjectIdentifier (type NAV_GOAL) (type-id 1) (description "Navigation Goal"))
  
  ; Small Black Aluminium Profile (class)
  (make-instance [F20_20_B] of ObjectIdentifier (type F20_20_B) (type-id 1) (description "Small Black Alu. Profile"))

  ; Small Grey Aluminium Profile (class)
  (make-instance [F20_20_G] of ObjectIdentifier (type F20_20_G) (type-id 1) (description "Small Grey Alu. Profile"))

  ; Large Black Aluminium Profile (class)
  (make-instance [S40_40_B] of ObjectIdentifier (type S40_40_B) (type-id 1) (description "Large Black Alu. Profile"))

  ; Large Grey Aluminium Profile (class)
  (make-instance [S40_40_G] of ObjectIdentifier (type S40_40_G) (type-id 1) (description "Large Grey Alu. Profile"))

  ; Small Grey Aluminium Profile (class)
  (make-instance [F20_20_G] of ObjectIdentifier (type M20_100) (type-id 1) (description "Bolt"))

  ; Small Grey Aluminium Profile (class)
  (make-instance [F20_20_G] of ObjectIdentifier (type M20) (type-id 1) (description "Small Nut"))


  ; Assembly aid trays (instances)
  (make-instance [em-01-01] of ObjectIdentifier (type EM) (type-id 1) (instance-id 1) (description "EM-01-01"))
  (make-instance [em-01-02] of ObjectIdentifier (type EM) (type-id 1) (instance-id 2) (description "EM-01-02"))
  (make-instance [em-01-03] of ObjectIdentifier (type EM) (type-id 1) (instance-id 3) (description "EM-01-03"))

  ; File card box (instances)
  (make-instance [em-02-01] of ObjectIdentifier (type EM) (type-id 2) (instance-id 1) (description "EM-02-01"))
  (make-instance [em-02-02] of ObjectIdentifier (type EM) (type-id 2) (instance-id 2) (description "EM-02-02"))
  (make-instance [em-02-03] of ObjectIdentifier (type EM) (type-id 2) (instance-id 3) (description "EM-02-03"))

  ; Foam container (instances)
  (make-instance [em-03-01] of ObjectIdentifier (type EM) (type-id 3) (instance-id 1) (description "EM-03-01"))

  ; Tray rack (instances)
  (make-instance [er-01-01] of ObjectIdentifier (type ER) (type-id 1) (instance-id 1) (description "ER-01-01"))

  ; Common shelf container (instances)
  (make-instance [er-02-01] of ObjectIdentifier (type ER) (type-id 2) (instance-id  1) (description "ER-02-01"))
  (make-instance [er-02-02] of ObjectIdentifier (type ER) (type-id 2) (instance-id  2) (description "ER-02-02"))
  (make-instance [er-03-03] of ObjectIdentifier (type ER) (type-id 2) (instance-id  3) (description "ER-02-03"))
  (make-instance [er-02-04] of ObjectIdentifier (type ER) (type-id 2) (instance-id  4) (description "ER-02-04"))
  (make-instance [er-02-05] of ObjectIdentifier (type ER) (type-id 2) (instance-id  5) (description "ER-02-05"))
  (make-instance [er-02-06] of ObjectIdentifier (type ER) (type-id 2) (instance-id  6) (description "ER-02-06"))
  (make-instance [er-02-07] of ObjectIdentifier (type ER) (type-id 2) (instance-id  7) (description "ER-02-07"))
  (make-instance [er-02-08] of ObjectIdentifier (type ER) (type-id 2) (instance-id  8) (description "ER-02-08"))
  (make-instance [er-02-09] of ObjectIdentifier (type ER) (type-id 2) (instance-id  9) (description "ER-02-09"))
  (make-instance [er-02-10] of ObjectIdentifier (type ER) (type-id 2) (instance-id 10) (description "ER-02-10"))
)
