;---------------------------------------------------------------------------
;  task-info.clp - AtWork RefBox CLIPS - task info specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass TaskInfo (is-a USER) (role concrete)
  ; The task info represents all currently existing tasks in the system

  (multislot tasks (type INSTANCE) (allowed-classes Task))
)

(defmessage-handler TaskInfo create-msg ()
  "Create a ProtoBuf message of an task info"

  (bind ?ti (pb-create "atwork_pb_msgs.TaskInfo"))

  (foreach ?task ?self:tasks
    (pb-add-list ?ti "tasks" (send ?task create-msg))
  )

  (return ?ti)
)


(defrule init-task-info
  (init)
  =>
  (make-instance [task-info] of TaskInfo)
)
