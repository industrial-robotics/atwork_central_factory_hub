;---------------------------------------------------------------------------
;  episode-serving-goodies.clp - AtWork RefBox CLIPS - Episode
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass EpisodeServingGoodies (is-a BenchmarkScenario) (role abstract) (pattern-match non-reactive))

(defmessage-handler EpisodeServingGoodies setup (?time ?state-machine)
  (make-instance [prep-timeup-state] of TimeoutState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-stopped-state] of StoppedState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time))
  (make-instance [prep-running-state] of RunningState
    (phase PREPARATION) (state-machine ?state-machine) (time ?time) (max-time ?*BMT-PREPARATION-TIME*))
  (make-instance [prep-paused-state] of PausedState
    (phase PREPARATION) (state-machine ?state-machine))

  (make-instance [exec-stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [exec-running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BMT-EXECUTION-TIME*))
  (make-instance [exec-paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [exec-finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [prep-stopped-state]    add-transition START           [prep-running-state])
  (send [prep-running-state]    add-transition PAUSE           [prep-paused-state])
  (send [prep-running-state]    add-transition STOP            [exec-stopped-state])
  (send [prep-running-state]    add-transition TIMEOUT         [prep-timeup-state])
  (send [prep-running-state]    add-transition FINISH          [prep-timeup-state])
  (send [prep-paused-state]     add-transition START           [prep-running-state])
  (send [prep-paused-state]     add-transition STOP            [exec-stopped-state])

  (send [prep-timeup-state]     add-transition START         [exec-running-state])

  (send [exec-stopped-state]    add-transition START           [exec-running-state])
  (send [exec-running-state]    add-transition PAUSE           [exec-paused-state])
  (send [exec-running-state]    add-transition STOP            [exec-finished-state])
  (send [exec-running-state]    add-transition TIMEOUT         [exec-finished-state])
  (send [exec-running-state]    add-transition FINISH          [exec-finished-state])
  (send [exec-paused-state]     add-transition START           [exec-running-state])
  (send [exec-paused-state]     add-transition STOP            [exec-stopped-state])

  (make-instance ?state-machine of StateMachine
    (current-state [prep-stopped-state])
    (states
      [prep-stopped-state] [prep-running-state] [prep-paused-state] [prep-finished-state]
      [exec-stopped-state] [exec-running-state] [exec-paused-state] [exec-finished-state]
    )
  )
)

(defmessage-handler EpisodeServingGoodies handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; Always finish the benchmark on feedback
)

;; BMT 1

(defclass EpisodeServingGoodies1 (is-a EpisodeServingGoodies) (role concrete))

(defmessage-handler EpisodeServingGoodies generate ()
  (printout t "Generating new EpisodeServingGoodies" crlf)

  (bind ?manipulation-chocolate-objects (create$ ?*CHOCOLATE-OBJECTS*))

  ; set static location for source
  (bind ?source-location [workstation-01])
  (bind ?shelf-location [shelf-01])
  ; set static location for destination
  (bind ?destination-location [workstation-02])

  ; 3 chocolates on flat surface
  (loop-for-count 3
    ; Pick random RoboCup object
    (bind ?item (pick-random$ ?manipulation-chocolate-objects))
    (bind ?manipulation-chocolate-objects (delete-member$ ?manipulation-chocolate-objects ?item))

    ; Add to inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item) (location-id ?source-location))
    )

    ; Manipulation Task
    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id ?item)
          (container-id [CONTAINER_B])
          (quantity-requested 1)
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
    )
  )


  ; 2 chocolates on shelf surface
  (loop-for-count 2
    ; Pick random chocolate object
    (bind ?item (pick-random$ ?manipulation-chocolate-objects))
    (bind ?manipulation-chocolate-objects (delete-member$ ?manipulation-chocolate-objects ?item))

    ; Add to inventory
    (slot-insert$ [inventory] items 1
      (make-instance of Item (object-id ?item) (location-id ?shelf-location))
    )

    ; Manipulation Task
    (slot-insert$ [task-info] tasks 1
      (make-instance of Task (status OFFERED) (task-type TRANSPORTATION)
        (transportation-task (make-instance of TransportationTask
          (object-id ?item)
          (quantity-requested 1)
          (container-id [CONTAINER_B])
          (destination-id ?destination-location)
          (source-id ?source-location)))
      )
    )
  )


  ; Add to container to inventory
  (slot-insert$ [inventory] items 1
    (make-instance of Item (object-id [CONTAINER_B]) (location-id ?destination-location))
  )
)


(defrule init-epi
  (init)
  ?epi <- (object (is-a Benchmark))
  =>
  (make-instance [EPI1] of EpisodeServingGoodies1 (type EPI) (type-id 1) (description "Episode Serving Goodies "))

  (slot-insert$ ?epi registered-scenarios 1 [EPI1])
)
