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

  (bind ?pb-object-identifier (pb-create "rockin_msgs.ObjectIdentifier"))

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
  ; Bearing box type A (class)
  (make-instance [ax-01] of ObjectIdentifier (type AX) (type-id 1) (description "AX-01"))

  ; Bearing (class)
  (make-instance [ax-02] of ObjectIdentifier (type AX) (type-id 2) (description "AX-02"))

  ; Axis (class)
  (make-instance [ax-03] of ObjectIdentifier (type AX) (type-id 3) (description "AX-03"))

  ; Shaft nut (class)
  (make-instance [ax-04] of ObjectIdentifier (type AX) (type-id 4) (description "AX-04"))

  ; Distance tube (class)
  (make-instance [ax-05] of ObjectIdentifier (type AX) (type-id 5) (description "AX-05"))

  ; Cover plate - faulty (class)
  (make-instance [ax-06] of ObjectIdentifier (type AX) (type-id 6) (description "AX-06"))

  ; Cover plate - machined (class)
  (make-instance [ax-07] of ObjectIdentifier (type AX) (type-id 7) (description "AX-07"))

  ; Cover plate - unusable(class)
  (make-instance [ax-08] of ObjectIdentifier (type AX) (type-id 8) (description "AX-08"))

  ; Motor with gearbox (class)
  (make-instance [ax-09] of ObjectIdentifier (type AX) (type-id 9) (description "AX-09"))

  ; Cover plate - unknown state (class)
  (make-instance [ax-15] of ObjectIdentifier (type AX) (type-id 15) (description "AX-15"))

  ; Bearing box type B (class)
  (make-instance [ax-16] of ObjectIdentifier (type AX) (type-id 16) (description "AX-16"))

  ; Assembly aid tray (class)
  (make-instance [em-01] of ObjectIdentifier (type EM) (type-id 1) (description "EM-01"))

  ; File card box (class)
  (make-instance [em-02] of ObjectIdentifier (type EM) (type-id 2) (description "EM-02"))

  ; Foam container (class)
  (make-instance [em-03] of ObjectIdentifier (type EM) (type-id 3) (description "EM-03"))

  ; Tray rack (class)
  (make-instance [er-01] of ObjectIdentifier (type ER) (type-id 1) (description "ER-01"))

  ; Common shelf container (class)
  (make-instance [er-02] of ObjectIdentifier (type ER) (type-id 2) (description "ER-02"))


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