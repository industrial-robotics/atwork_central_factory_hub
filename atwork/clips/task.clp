;---------------------------------------------------------------------------
;  task.clp - RoCKIn RefBox CLIPS - task specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass Task (is-a USER) (role concrete)
  (slot next-id (type INTEGER) (storage shared) (default 0))

  (slot id (type INTEGER))
  (slot task-type (type SYMBOL) (allowed-values UNKNOWN TRANSPORTATION NAVIGATION))
  (slot status (type SYMBOL) (allowed-values OFFERED TIMEOUT IN_PROGRESS PAUSED ABORTED FINISHED))
  (multislot transportation-task (type INSTANCE) (allowed-classes TransportationTask) (cardinality 0 1))
  (multislot navigation-task (type INSTANCE) (allowed-classes NavigationTask) (cardinality 0 1))
)

(defmessage-handler Task init ()
  ; first, initialize the other slots
  (call-next-handler)

  ; then, overwrite the id slot from the class variable and increase the next id
  (modify-instance ?self (id ?self:next-id) (next-id (+ ?self:next-id 1)))
)

(defmessage-handler Task create-msg ()
  "Create a ProtoBuf message of an task"

  (bind ?o (pb-create "atwork_pb_msgs.Task"))

  (pb-set-field ?o "id" ?self:id)
  (pb-set-field ?o "type" ?self:task-type)
  (pb-set-field ?o "status" ?self:status)

  (if (<> (length$ ?self:transportation-task) 0) then
    (bind ?tt (send (nth$ 1 ?self:transportation-task) create-msg))
    (pb-set-field ?o "transportation_task" ?tt)  ; destroys ?tt
  )

  (if (<> (length$ ?self:navigation-task) 0) then
    (bind ?nt (send (nth$ 1 ?self:navigation-task) create-msg))
    (pb-set-field ?o "navigation_task" ?nt)  ; destroys ?nt
  )

  (return ?o)
)


; When all instances of a class are deleted, the shared slots are reset.
; Prevent this by creating a dummy task, which does not get deleted.
(defrule make-dummy-task
  (init)
  =>
  (make-instance [dummy-task] of Task)
)
