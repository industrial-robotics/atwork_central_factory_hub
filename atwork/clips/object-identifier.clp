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
    PICKUP_BLACK_WHITE       ;
    PICKUP_CHOCO_MILK        ;
    DUPLO_WHITE              ;
    DUPLO_CLASSIC            ;
    TWIX_SPEKULATIUS         ;
    TWIX_WHITE               ;
    TWIX_CLASSIC             ;
    TWIX_MINI                ;
    MARS                     ;
    MARS_MINI                ;
    SNICKERS                 ;
    SNICKERS_MINI            ;
    KITKAT_CLASSIC           ;
    KITKAT_WHITE             ;
    KITKAT_CHUNKY_WHITE      ;
    KITKAT_CHUNKY_CLASSIC    ;
    KITKAT_MINI              ;
    LION_CLASSIC             ;
    LION_MINI                ;
    M_M_CRIPSY               ;
    M_M_PEANUT               ;
    BOUNTY                   ;
    BOUNTY_MINI              ;
    MILKYWAY                 ;
    MILKYWAY_MINI            ;
    RITTERSPORT_NUGAT        ;
    RITTERSPORT_KNUSPERKEKS  ;
    RITTERSPORT_JOGHURT      ;
    RITTERSPORT_KNUSPERFLAKES;
    RITTERSPORT_NUSS_SPLITTER;
    RITTERSPORT_MARZIPAN     ;

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
  (make-instance [PICKUP_BLACK_WHITE]        of ObjectIdentifier (type PICKUP_BLACK_WHITE)        (type-id 1) (description "PICKUP_BLACK_WHITE"))
  (make-instance [PICKUP_CHOCO_MILK]         of ObjectIdentifier (type PICKUP_CHOCO_MILK)         (type-id 1) (description "PICKUP_CHOCO_MILK"))
  (make-instance [DUPLO_WHITE]               of ObjectIdentifier (type DUPLO_WHITE)               (type-id 1) (description "DUPLO_WHITE"))
  (make-instance [DUPLO_CLASSIC]             of ObjectIdentifier (type DUPLO_CLASSIC)             (type-id 1) (description "DUPLO_CLASSIC"))
  (make-instance [TWIX_SPEKULATIUS]          of ObjectIdentifier (type TWIX_SPEKULATIUS)          (type-id 1) (description "TWIX_SPEKULATIUS"))
  (make-instance [TWIX_WHITE]                of ObjectIdentifier (type TWIX_WHITE)                (type-id 1) (description "TWIX_WHITE"))
  (make-instance [TWIX_CLASSIC]              of ObjectIdentifier (type TWIX_CLASSIC)              (type-id 1) (description "TWIX_CLASSIC"))
  (make-instance [TWIX_MINI]                 of ObjectIdentifier (type TWIX_MINI)                 (type-id 1) (description "TWIX_MINI"))
  (make-instance [MARS]                      of ObjectIdentifier (type MARS)                      (type-id 1) (description "MARS"))
  (make-instance [MARS_MINI]                 of ObjectIdentifier (type MARS_MINI)                 (type-id 1) (description "MARS_MINI"))
  (make-instance [SNICKERS]                  of ObjectIdentifier (type SNICKERS)                  (type-id 1) (description "SNICKERS"))
  (make-instance [SNICKERS_MINI]             of ObjectIdentifier (type SNICKERS_MINI)             (type-id 1) (description "SNICKERS_MINI"))
  (make-instance [KITKAT_CLASSIC]            of ObjectIdentifier (type KITKAT_CLASSIC)            (type-id 1) (description "KITKAT_CLASSIC"))
  (make-instance [KITKAT_WHITE]              of ObjectIdentifier (type KITKAT_WHITE)              (type-id 1) (description "KITKAT_WHITE"))
  (make-instance [KITKAT_CHUNKY_WHITE]       of ObjectIdentifier (type KITKAT_CHUNKY_WHITE)       (type-id 1) (description "KITKAT_CHUNKY_WHITE"))
  (make-instance [KITKAT_CHUNKY_CLASSIC]     of ObjectIdentifier (type KITKAT_CHUNKY_CLASSIC)     (type-id 1) (description "KITKAT_CHUNKY_CLASSIC"))
  (make-instance [KITKAT_MINI]               of ObjectIdentifier (type KITKAT_MINI)               (type-id 1) (description "KITKAT_MINI"))
  (make-instance [LION_CLASSIC]              of ObjectIdentifier (type LION_CLASSIC)              (type-id 1) (description "LION_CLASSIC"))
  (make-instance [LION_MINI]                 of ObjectIdentifier (type LION_MINI)                 (type-id 1) (description "LION_MINI"))
  (make-instance [M_M_CRIPSY]                of ObjectIdentifier (type M_M_CRIPSY)                (type-id 1) (description "M_M_CRIPSY"))
  (make-instance [M_M_PEANUT]                of ObjectIdentifier (type M_M_PEANUT)                (type-id 1) (description "M_M_PEANUT"))
  (make-instance [BOUNTY]                    of ObjectIdentifier (type BOUNTY)                    (type-id 1) (description "BOUNTY"))
  (make-instance [BOUNTY_MINI]               of ObjectIdentifier (type BOUNTY_MINI)               (type-id 1) (description "BOUNTY_MINI"))
  (make-instance [MILKYWAY]                  of ObjectIdentifier (type MILKYWAY)                  (type-id 1) (description "MILKYWAY"))
  (make-instance [MILKYWAY_MINI]             of ObjectIdentifier (type MILKYWAY_MINI)             (type-id 1) (description "MILKYWAY_MINI"))
  (make-instance [RITTERSPORT_NUGAT]         of ObjectIdentifier (type RITTERSPORT_NUGAT)         (type-id 1) (description "RITTERSPORT_NUGAT"))
  (make-instance [RITTERSPORT_KNUSPERKEKS]   of ObjectIdentifier (type RITTERSPORT_KNUSPERKEKS)   (type-id 1) (description "RITTERSPORT_KNUSPERKEKS"))
  (make-instance [RITTERSPORT_JOGHURT]       of ObjectIdentifier (type RITTERSPORT_JOGHURT)       (type-id 1) (description "RITTERSPORT_JOGHURT"))
  (make-instance [RITTERSPORT_KNUSPERFLAKES] of ObjectIdentifier (type RITTERSPORT_KNUSPERFLAKES) (type-id 1) (description "RITTERSPORT_KNUSPERFLAKES"))
  (make-instance [RITTERSPORT_NUSS_SPLITTER] of ObjectIdentifier (type RITTERSPORT_NUSS_SPLITTER) (type-id 1) (description "RITTERSPORT_NUSS_SPLITTER"))
  (make-instance [RITTERSPORT_MARZIPAN]      of ObjectIdentifier (type RITTERSPORT_MARZIPAN)      (type-id 1) (description "RITTERSPORT_MARZIPAN"))
)
