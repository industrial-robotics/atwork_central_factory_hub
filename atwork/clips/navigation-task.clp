;---------------------------------------------------------------------------
;  navigation-task.clp - RoCKIn RefBox CLIPS - navigation-task specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass NavigationTask (is-a USER) (role concrete)
  (slot location-id (type INSTANCE) (allowed-classes LocationIdentifier))
  (slot wait-time (type INTEGER))
  (slot orientation (type SYMBOL) (allowed-values NORTH EAST SOUTH WEST))
)

(defmessage-handler NavigationTask create-msg ()
  "Create a ProtoBuf message of an navigation-task"

  (bind ?nt (pb-create "atwork_pb_msgs.NavigationTask"))

  (bind ?li (send ?self:location-id create-msg))
  (pb-set-field ?nt "location" ?li) ; destroys ?li

  (bind ?nt-time (pb-field-value ?nt "wait_time"))

  ; Set the wait time (in seconds)
  (if (eq (type ?nt-time) EXTERNAL-ADDRESS) then
    (bind ?time (time-from-sec ?self:wait-time))
    (pb-set-field ?nt-time "sec" (nth$ 1 ?time))
    (pb-set-field ?nt-time "nsec" (integer (* (nth$ 2 ?time) 1000)))
    (pb-set-field ?nt "wait_time" ?nt-time) ; destroys ?nt-time!
  )

  (pb-set-field ?nt "orientation" ?self:orientation)

  (return ?nt)
)
