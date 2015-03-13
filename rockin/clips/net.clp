;---------------------------------------------------------------------------
;  net.clp - RoCKIn RefBox CLIPS network handling
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deffunction net-create-VersionInfo ()
  (bind ?vi (pb-create "rockin_msgs.VersionInfo"))
  (pb-set-field ?vi "version_major" ?*VERSION-MAJOR*)
  (pb-set-field ?vi "version_minor" ?*VERSION-MINOR*)
  (pb-set-field ?vi "version_micro" ?*VERSION-MICRO*)
  (pb-set-field ?vi "version_string"
    (str-cat ?*VERSION-MAJOR* "." ?*VERSION-MINOR* "." ?*VERSION-MICRO*))
  (return ?vi)
)

(deffunction net-init-peer (?cfg-prefix ?group)
  (bind ?peer-id 0)

  (do-for-fact ((?csp confval) (?crp confval) (?ch confval))
    (and (eq ?csp:type UINT) (eq ?csp:path (str-cat ?cfg-prefix "send-port"))
        (eq ?crp:type UINT) (eq ?crp:path (str-cat ?cfg-prefix "recv-port"))
        (eq ?ch:type STRING) (eq ?ch:path (str-cat ?cfg-prefix "host")))
    (printout t "Creating local communication peer for group " ?group
        " (send port " ?csp:value "  recv port " ?crp:value ")" crlf)
    (bind ?peer-id (pb-peer-create-local ?ch:value ?csp:value ?crp:value))
  )
  (if (eq ?peer-id 0)
   then
    (do-for-fact ((?cp confval) (?ch confval))
      (and (eq ?cp:type UINT) (eq ?cp:path (str-cat ?cfg-prefix "port"))
          (eq ?ch:type STRING) (eq ?ch:path (str-cat ?cfg-prefix "host")))
      (printout t "Creating communication peer for group " ?group
          " (port " ?cp:value ")" crlf)
      (bind ?peer-id (pb-peer-create ?ch:value ?cp:value))
    )
  )

  (if (neq ?peer-id 0)
   then
    (assert (network-peer (group ?group) (id ?peer-id)))
   else
    (printout warn "No network configuration found for " ?group " at " ?cfg-prefix crlf)
  )
)

(defrule net-init-public
  (init)
  (config-loaded)
  (not (network-peer (group "PUBLIC")))
  =>
  (net-init-peer "/llsfrb/comm/public-peer/" "PUBLIC")
)

(defrule net-init-peers
  (init)
  (config-loaded)
  (known-team (name ?team))
  (not (network-peer (group ?team)))
  =>
  (net-init-peer (str-cat "/llsfrb/comm/" ?team "-peer/") ?team)
)

(defrule net-read-known-teams
  (declare (salience -1000))
  (init)
  (confval (path "/llsfrb/game/teams") (type STRING) (is-list TRUE) (list-value $?lv))
  =>
  (printout t "Teams: " ?lv crlf)
  (foreach ?team ?lv
    (assert (known-team (name ?team)))
  )
)

(defrule net-client-connected
  ?cf <- (protobuf-server-client-connected ?client-id ?host ?port)
  =>
  (retract ?cf)
  (assert (network-client (id ?client-id) (host ?host) (port ?port)))
  (printout t "Client " ?client-id " connected from " ?host ":" ?port crlf)

  ; Send version information right away
  (bind ?vi (net-create-VersionInfo))
  (pb-send ?client-id ?vi)
  (pb-destroy ?vi)
)

(defrule net-client-disconnected
  ?cf <- (protobuf-server-client-disconnected ?client-id)
  ?nf <- (network-client (id ?client-id) (host ?host))
  =>
  (retract ?cf ?nf)
  (printout t "Client " ?client-id " ( " ?host ") disconnected" crlf)
)

(defrule net-send-beacon
  (time $?now)
  ?f <- (signal (type beacon) (time $?t&:(timeout ?now ?t ?*BEACON-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))
  (if (debug 3) then (printout t "Sending beacon" crlf))
  (bind ?beacon (pb-create "rockin_msgs.BeaconSignal"))
  (bind ?beacon-time (pb-field-value ?beacon "time"))
  (pb-set-field ?beacon-time "sec" (nth$ 1 ?now))
  (pb-set-field ?beacon-time "nsec" (integer (* (nth$ 2 ?now) 1000)))
  (pb-set-field ?beacon "time" ?beacon-time) ; destroys ?beacon-time!
  (pb-set-field ?beacon "seq" ?seq)
  (pb-set-field ?beacon "team_name" "RoCKIn")
  (pb-set-field ?beacon "peer_name" "RefBox")
  (pb-broadcast ?peer-id-public ?beacon)
  (pb-destroy ?beacon)
)

(defrule net-recv-beacon-known
  ?mf <- (protobuf-msg (type "rockin_msgs.BeaconSignal") (ptr ?p) (rcvd-at $?rcvd-at)
           (rcvd-from ?from-host ?from-port) (rcvd-via ?via))
  ?rf <- (robot (host ?from-host) (port ?from-port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (printout debug "Received beacon from known " ?from-host ":" ?from-port crlf)
  (bind ?team (pb-field-value ?p "team_name"))
  (bind ?name (pb-field-value ?p "peer_name"))
  (bind ?time (pb-field-value ?p "time"))

  (modify ?rf (last-seen ?rcvd-at) (warning-sent FALSE))
)

(defrule net-recv-beacon-unknown
  ?mf <- (protobuf-msg (type "rockin_msgs.BeaconSignal") (ptr ?p) (rcvd-at $?rcvd-at)
           (rcvd-from ?from-host ?from-port) (rcvd-via ?via))
  (not (robot (host ?from-host) (port ?from-port)))
  ?sf <- (signal (type version-info))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (modify ?sf (count 0) (time 0 0))
  (printout debug "Received initial beacon from " ?from-host ":" ?from-port crlf)
  (bind ?team (pb-field-value ?p "team_name"))
  (bind ?name (pb-field-value ?p "peer_name"))
  (bind ?timef (pb-field-value ?p "time"))
  (bind ?time (create$ (pb-field-value ?timef "sec") (integer (/ (pb-field-value ?timef "nsec") 1000))))
  (bind ?peer-time-diff (abs (time-diff-sec ?rcvd-at ?time)))
  (if (> ?peer-time-diff ?*PEER-TIME-DIFFERENCE-WARNING*)
   then
    (printout warn "Robot " ?name " of " ?team
        " has a large time offset (" ?peer-time-diff " sec)" crlf)
    (assert (attention-message (text (str-cat "Robot " ?name " of " ?team
                " has a large time offset ("
                ?peer-time-diff " sec)"))))
  )
  (do-for-fact ((?other robot)) (eq ?other:host ?from-host)
    (printout warn "Received two BeaconSignals from host " ?from-host
        " (" ?other:team "/" ?other:name "@" ?other:port " vs "
        ?team "/" ?from-host "@" ?from-port ")" crlf)
    (assert (attention-message (text (str-cat "Received two BeaconSignals form host "
                ?from-host " (" ?other:team "/" ?other:name
                "@" ?other:port " vs " ?team "/" ?from-host
                "@" ?from-port ")"))))
  )

  (if (and (eq ?team "RoCKIn") (eq ?name "RefBox"))
   then
    (printout warn "Detected another RefBox at " ?from-host ":" ?from-port crlf)
    (assert (attention-message (text (str-cat "Detected another RefBox at "
                ?from-host ":" ?from-port))))
  )
  (assert (robot (team ?team) (name ?name) (host ?from-host) (port ?from-port)
    (last-seen ?rcvd-at)))
)

(defrule send-attmsg
  ?af <- (attention-message (text ?text) (team ?team) (time ?time-to-show))
  =>
  (retract ?af)
  (bind ?attmsg (pb-create "rockin_msgs.AttentionMessage"))
  (pb-set-field ?attmsg "message" (str-cat ?text))
  (if (neq (str-compare ?team "") 0) then (pb-set-field ?attmsg "team" ?team))
  (if (> ?time-to-show 0) then
    (pb-set-field ?attmsg "time_to_show" ?time-to-show))

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?attmsg))
  (pb-destroy ?attmsg)
)

(defrule net-recv-SetBenchmarkState
  ?sf <- (benchmark-state (state ?state))
  ?mf <- (protobuf-msg (type "rockin_msgs.SetBenchmarkState") (ptr ?p) (rcvd-via STREAM))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (modify ?sf (state (sym-cat (pb-field-value ?p "state"))) (prev-state ?state))
)

(defrule net-recv-SetBenchmarkState-illegal
  ?mf <- (protobuf-msg (type "rockin_msgs.SetBenchmarkState") (ptr ?p)
           (rcvd-via BROADCAST) (rcvd-from ?host ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (printout warn "Illegal SetBenchmarkState message received from host " ?host crlf)
)

(defrule net-recv-SetBenchmarkPhase
  ?sf <- (benchmark-state (phase-id ?phase-id))
  ?mf <- (protobuf-msg (type "rockin_msgs.SetBenchmarkPhase") (ptr ?p) (rcvd-via STREAM))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the phase type (NONE, FBM, TBM) and type id from the message
  (bind ?pb-phase (pb-field-value ?p "phase"))
  (bind ?pb-phase-type (pb-field-value ?pb-phase "type"))
  (bind ?pb-phase-type-id (pb-field-value ?pb-phase "type_id"))

  ; When a phase was found, that matches the type and ID in the message, update the current and previous phase ID
  (do-for-fact ((?phase benchmark-phase)) (and (eq ?phase:type ?pb-phase-type) (eq ?phase:type-id ?pb-phase-type-id))
    (modify ?sf (phase-id ?phase:id) (prev-phase-id ?phase-id))
  )
)

(defrule net-recv-SetBenchmarkPhase-illegal
  ?mf <- (protobuf-msg (type "rockin_msgs.SetBenchmarkPhase") (ptr ?p)
           (rcvd-via BROADCAST) (rcvd-from ?host ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (printout warn "Illegal SetBenchmarkPhase message received from host " ?host crlf)
)

(defrule net-recv-SetBenchmarkTransitionEvent
  ?sf <- (benchmark-state (phase-id ?phase-id))
  ?mf <- (protobuf-msg (type "rockin_msgs.SetBenchmarkTransitionEvent") (ptr ?p)
           (rcvd-via STREAM) (rcvd-from ?host ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (bind ?pb-event (pb-field-value ?p "event"))

  (if (eq ?pb-event RESET) then
    (do-for-fact ((?phase benchmark-phase)) (eq ?phase:id ?phase-id)
      (if (and (eq ?phase:type FBM) (eq ?phase:type-id 1)) then (functionality-benchmarks-fbm1-init))
      (if (and (eq ?phase:type FBM) (eq ?phase:type-id 2)) then (functionality-benchmarks-fbm2-init))
      (if (and (eq ?phase:type TBM) (eq ?phase:type-id 1)) then (task-benchmarks-tbm1-init))
      (if (and (eq ?phase:type TBM) (eq ?phase:type-id 2)) then (task-benchmarks-tbm2-init))
      (if (and (eq ?phase:type TBM) (eq ?phase:type-id 3)) then (task-benchmarks-tbm3-init))
    )
  else
    (send [sm] process-event ?pb-event)
  )
)

(defrule net-recv-SetBenchmarkTransitionEvent-illegal
  ?mf <- (protobuf-msg (type "rockin_msgs.SetBenchmarkTransitionEvent") (ptr ?p)
           (rcvd-via BROADCAST) (rcvd-from ?host ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (printout warn "Illegal SetBenchmarkTransitionEvent message received from host " ?host crlf)
)

(deffunction net-create-BenchmarkState (?bs)
  (bind ?benchmarkstate (pb-create "rockin_msgs.BenchmarkState"))
  (bind ?benchmarkstate-time (pb-field-value ?benchmarkstate "benchmark_time"))
  (bind ?benchmarkstate-phase (pb-field-value ?benchmarkstate "phase"))

  ; Set the benchmark time (in seconds)
  (if (eq (type ?benchmarkstate-time) EXTERNAL-ADDRESS) then
    (bind ?gt (time-from-sec (fact-slot-value ?bs benchmark-time)))
    (pb-set-field ?benchmarkstate-time "sec" (nth$ 1 ?gt))
    (pb-set-field ?benchmarkstate-time "nsec" (integer (* (nth$ 2 ?gt) 1000)))
    (pb-set-field ?benchmarkstate "benchmark_time" ?benchmarkstate-time) ; destroys ?benchmarkstate-time!
  )

  ; Add the current phase of the benchmark
  (do-for-fact ((?phase benchmark-phase)) (eq ?phase:id (fact-slot-value ?bs phase-id))
    (pb-set-field ?benchmarkstate-phase "type" ?phase:type)
    (pb-set-field ?benchmarkstate-phase "type_id" ?phase:type-id)
    (pb-set-field ?benchmarkstate-phase "description" ?phase:description)
  )
  (pb-set-field ?benchmarkstate "phase" ?benchmarkstate-phase)

  ; Add all known teams
  (do-for-all-facts ((?team known-team)) TRUE
    (pb-add-list ?benchmarkstate "known_teams" ?team:name)
  )

  ; Set the benchmark state (e.g. PAUSED or RUNNING) based on the state machine
  (bind ?current-state (send [sm] get-current-state))
  (bind ?robot-state (send ?current-state to-robot-state))
  (pb-set-field ?benchmarkstate "state" ?robot-state)

  (return ?benchmarkstate)
)

(defrule net-send-BenchmarkState
  (time $?now)
  ?bs <- (benchmark-state (refbox-mode ?refbox-mode))
  ?f <- (signal (type benchmark-state) (time $?t&:(timeout ?now ?t ?*BENCHMARKSTATE-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))
  (if (debug 3) then (printout t "Sending BenchmarkState" crlf))
  (bind ?benchmark-state (net-create-BenchmarkState ?bs))

  (pb-broadcast ?peer-id-public ?benchmark-state)

  ; For stream clients set refbox mode
  (pb-set-field ?benchmark-state "refbox_mode" ?refbox-mode)

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?benchmark-state)
  )
  (pb-destroy ?benchmark-state)
)

(deffunction net-create-RobotInfo ()
  (bind ?ri (pb-create "rockin_msgs.RobotInfo"))

  (do-for-all-facts ((?robot robot)) TRUE
    (bind ?r (pb-create "rockin_msgs.Robot"))
    (bind ?r-time (pb-field-value ?r "last_seen"))
    (if (eq (type ?r-time) EXTERNAL-ADDRESS) then
      (pb-set-field ?r-time "sec" (nth$ 1 ?robot:last-seen))
      (pb-set-field ?r-time "nsec" (integer (* (nth$ 2 ?robot:last-seen) 1000)))
      (pb-set-field ?r "last_seen" ?r-time) ; destroys ?r-time!
    )

    (pb-set-field ?r "name" ?robot:name)
    (pb-set-field ?r "team" ?robot:team)
    (pb-set-field ?r "host" ?robot:host)

    (pb-add-list ?ri "robots" ?r) ; destroys ?r
  )

  (return ?ri)
)

(defrule net-send-RobotInfo
  (time $?now)
  ?f <- (signal (type robot-info) (time $?t&:(timeout ?now ?t ?*ROBOTINFO-PERIOD*)) (seq ?seq))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))
  (bind ?ri (net-create-RobotInfo))


  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?ri)
  )

  (pb-destroy ?ri)
)

(defrule net-send-Inventory
  (time $?now)
  ?f <- (signal (type inventory) (time $?t&:(timeout ?now ?t ?*INVENTORY-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))

  (bind ?pb-inventory (send [inventory] create-msg))
  (pb-broadcast ?peer-id-public ?pb-inventory)

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?pb-inventory)
  )

  (pb-destroy ?pb-inventory)
)

(defrule net-send-OrderInfo
  (time $?now)
  ?sf <- (signal (type order-info) (seq ?seq) (count ?count)
     (time $?t&:(timeout ?now ?t (if (> ?count ?*BC-ORDERINFO-BURST-COUNT*)
                 then ?*BC-ORDERINFO-PERIOD*
                 else ?*BC-ORDERINFO-BURST-PERIOD*))))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?sf (time ?now) (seq (+ ?seq 1)) (count (+ ?count 1)))

  (bind ?oi (send [order-info] create-msg))
  (pb-broadcast ?peer-id-public ?oi)

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?oi)
  )

  (pb-destroy ?oi)
)

(defrule net-send-VersionInfo
  (time $?now)
  ?sf <- (signal (type version-info) (seq ?seq)
     (count ?count&:(< ?count ?*BC-VERSIONINFO-COUNT*))
     (time $?t&:(timeout ?now ?t ?*BC-VERSIONINFO-PERIOD*)))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?sf (time ?now) (seq (+ ?seq 1)) (count (+ ?count 1)))
  (bind ?vi (net-create-VersionInfo))
  (pb-broadcast ?peer-id-public ?vi)
  (pb-destroy ?vi)
)

(defrule net-recv-DrillingMachineCommand
  ?mf <- (protobuf-msg (type "rockin_msgs.DrillingMachineCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (robot (name ?name) (team ?team) (host ?host))
  (have-feature DrillingMachine)
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case MOVE_UP then
      (printout t "Robot " ?name "/" ?team " commands drilling machine up" crlf)
      (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands drilling machine up"))))
      (drilling-machine-move-drill-up)
    )

    (case MOVE_DOWN then
      (printout t "Robot " ?name "/" ?team " commands drilling machine down" crlf)
      (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands drilling machine down"))))
      (drilling-machine-move-drill-down)
    )
  )
)

(defrule net-recv-DrillingMachineCommand-client
  ?mf <- (protobuf-msg (type "rockin_msgs.DrillingMachineCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (network-client (id ?client-id) (host ?client-host) (port ?port))
  (have-feature DrillingMachine)
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case MOVE_UP then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands drilling machine up" crlf)
      (drilling-machine-move-drill-up)
    )

    (case MOVE_DOWN then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands drilling machine down" crlf)
      (drilling-machine-move-drill-down)
    )
  )
)

(deffunction net-create-DrillingMachineStatus ()
  ; Instantiate a new status message
  (bind ?pb-status (pb-create "rockin_msgs.DrillingMachineStatus"))

  ; Assumption: The enum in the device communication message is aligned with the
  ;             enum of the robot communication (e.g. in both messages the state
  ;             UNKNOWN maps to the value 4)
  (switch (drilling-machine-get-state)
    (case 0 then (pb-set-field ?pb-status "state" AT_TOP))
    (case 1 then (pb-set-field ?pb-status "state" AT_BOTTOM))
    (case 2 then (pb-set-field ?pb-status "state" MOVING_UP))
    (case 3 then (pb-set-field ?pb-status "state" MOVING_DOWN))
    (case 4 then (pb-set-field ?pb-status "state" UNKNOWN))
  )

  (return ?pb-status)
)

(defrule net-send-DrillingMachineStatus
  (time $?now)
  ?f <- (signal (type drilling-machine) (time $?t&:(timeout ?now ?t ?*DRILLING-MACHINE-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  (have-feature DrillingMachine)
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))

  (bind ?ds (net-create-DrillingMachineStatus))

  ; Broadcast to peers
  (pb-broadcast ?peer-id-public ?ds)

  ; Send to all clients
  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?ds)
  )

  (pb-destroy ?ds)
)

(defrule net-recv-ConveyorBeltCommand
  ?mf <- (protobuf-msg (type "rockin_msgs.ConveyorBeltCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (robot (name ?name) (team ?team) (host ?host))
  (have-feature ConveyorBelt)
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case STOP then
      (printout t "Robot " ?name "/" ?team " commands conveyor belt to stop" crlf)
      (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands conveyor belt to stop"))))
      (conveyor-belt-stop-belt)
    )

    (case START then
      (printout t "Robot " ?name "/" ?team " commands conveyor belt to start" crlf)
      (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " commands conveyor belt to start"))))
      (conveyor-belt-start-belt)
    )
  )
)

(defrule net-recv-ConveyorBeltCommand-client
  ?mf <- (protobuf-msg (type "rockin_msgs.ConveyorBeltCommand") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (network-client (id ?client-id) (host ?client-host) (port ?port))
  (have-feature ConveyorBelt)
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the command from the message
  (bind ?pb-command (pb-field-value ?p "command"))

  (switch ?pb-command
    (case STOP then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands conveyor belt to stop" crlf)
      (conveyor-belt-stop-belt)
    )

    (case START then
      (printout t "Client " ?client-id " (" ?client-host ":" ?port ") commands conveyor belt to start" crlf)
      (conveyor-belt-start-belt)
    )
  )
)

(deffunction net-create-ConveyorBeltStatus ()
  ; Instantiate a new status message
  (bind ?pb-status (pb-create "rockin_msgs.ConveyorBeltStatus"))

  (if (conveyor-belt-is-running)
   then
    (pb-set-field ?pb-status "state" START)
   else
    (pb-set-field ?pb-status "state" STOP)
  )

  (return ?pb-status)
)

(defrule net-send-ConveyorBeltStatus
  (time $?now)
  ?f <- (signal (type conveyor-belt) (time $?t&:(timeout ?now ?t ?*CONVEYOR-BELT-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  (have-feature ConveyorBelt)
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))

  (bind ?status (net-create-ConveyorBeltStatus))

  ; Broadcast to peers
  (pb-broadcast ?peer-id-public ?status)

  ; Send to all clients
  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?status)
  )

  (pb-destroy ?status)
)

(defrule net-recv-CameraCommand
  ?mf <- (protobuf-msg (type "rockin_msgs.CameraCommand") (rcvd-from ?host ?port))
  (network-peer (group ?group) (id ?peer-id))
  (robot (name ?name) (team ?team) (host ?host))
  (have-feature QualityControlCamera)
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (printout t "Robot " ?name "/" ?team " requests camera image" crlf)
  (assert (attention-message (text (str-cat "Robot " ?name "/" ?team " requests camera image"))))

  (quality-control-camera-send-image-to-peer ?peer-id)
)

(defrule net-recv-BenchmarkFeedback-fbm1-peer
  ?mf <- (protobuf-msg (type "rockin_msgs.BenchmarkFeedback") (ptr ?p)
          (rcvd-at $?rcvd-at) (rcvd-from ?from-host ?from-port) (client-type PEER))
  (robot (name ?name) (team ?team) (host ?from-host))
  (benchmark-phase (id ?phase) (type FBM) (type-id 1))
  (benchmark-state (phase-id ?phase) (state RUNNING))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (assert (benchmark-run-over))

  (if (and
       (pb-has-field ?p "object_class_name")
       (pb-has-field ?p "object_instance_name")
       (pb-has-field ?p "object_pose"))
   then
    (printout t "FBM: Robot " ?name "/" ?team
        " recognized object instance " (pb-field-value ?p "object_instance_name")
        " of class " (pb-field-value ?p "object_class_name") crlf)
    (assert (attention-message (text (str-cat "FBM: Robot " ?name "/" ?team
        " recognized object instance" (pb-field-value ?p "object_instance_name")
        " of class " (pb-field-value ?p "object_class_name")))))

    (bind ?pose (pb-field-value ?p "object_pose"))
    (bind ?position (pb-field-value ?pose "position"))
    (bind ?orientation (pb-field-value ?pose "orientation"))

    (assert (benchmark-feedback (time ?rcvd-at) (type RECOGNIZED)
        (object-class-name (pb-field-value ?p "object_class_name"))
        (object-instance-name (pb-field-value ?p "object_instance_name"))
        (object-pose-position-x (pb-field-value ?position "x"))
        (object-pose-position-y (pb-field-value ?position "y"))
        (object-pose-position-z (pb-field-value ?position "z"))
        (object-pose-orientation-x (pb-field-value ?orientation "x"))
        (object-pose-orientation-y (pb-field-value ?orientation "y"))
        (object-pose-orientation-z (pb-field-value ?orientation "z"))
        (object-pose-orientation-w (pb-field-value ?orientation "w"))))
   else
    (printout t "Benchmark feedback from " ?from-host ":" ?from-port " is invalid" crlf)
    (assert (benchmark-feedback (time ?rcvd-at) (type TIMEOUT)))
  )
)

(defrule net-recv-BenchmarkFeedback-fbm2-peer
  ?mf <- (protobuf-msg (type "rockin_msgs.BenchmarkFeedback") (ptr ?p)
          (rcvd-at $?rcvd-at) (rcvd-from ?from-host ?from-port) (client-type PEER))
  (robot (name ?name) (team ?team) (host ?from-host))
  (benchmark-phase (id ?phase) (type FBM) (type-id 2))
  (benchmark-state (phase-id ?phase) (state RUNNING))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (assert (benchmark-run-over))

  (if (and
       (pb-has-field ?p "grasp_notification")
       (pb-has-field ?p "object_class_name")
       (pb-has-field ?p "object_instance_name")
       (pb-has-field ?p "end_effector_pose"))
   then
    (printout t "FBM: Robot " ?name "/" ?team
        " lifted object instance " (pb-field-value ?p "object_instance_name")
        " of class " (pb-field-value ?p "object_class_name") crlf)
    (assert (attention-message (text (str-cat "FBM: Robot " ?name "/" ?team
        " lifted object instance " (pb-field-value ?p "object_instance_name")
        " of class " (pb-field-value ?p "object_class_name")))))

    (bind ?pose (pb-field-value ?p "end_effector_pose"))
    (bind ?position (pb-field-value ?pose "position"))
    (bind ?orientation (pb-field-value ?pose "orientation"))

    (assert (benchmark-feedback (source PEER) (time ?rcvd-at) (type LIFTED)
        (grasp-notification (pb-field-value ?p "grasp_notification"))
        (object-class-name (pb-field-value ?p "object_class_name"))
        (object-instance-name (pb-field-value ?p "object_instance_name"))
        (end-effector-pose-position-x (pb-field-value ?position "x"))
        (end-effector-pose-position-y (pb-field-value ?position "y"))
        (end-effector-pose-position-z (pb-field-value ?position "z"))
        (end-effector-pose-orientation-x (pb-field-value ?orientation "x"))
        (end-effector-pose-orientation-y (pb-field-value ?orientation "y"))
        (end-effector-pose-orientation-z (pb-field-value ?orientation "z"))
        (end-effector-pose-orientation-w (pb-field-value ?orientation "w"))))
   else
    (printout t "Benchmark feedback from " ?from-host ":" ?from-port " is invalid" crlf)
    (assert (benchmark-feedback (time ?rcvd-at) (type TIMEOUT)))
  )
)

(defrule net-recv-BenchmarkFeedback-fbm2-client
  ?mf <- (protobuf-msg (type "rockin_msgs.BenchmarkFeedback") (ptr ?p)
          (rcvd-at $?rcvd-at) (rcvd-from ?from-host ?from-port) (client-type CLIENT|SERVER))
  (benchmark-phase (id ?phase) (type FBM) (type-id 2))
  (benchmark-state (phase-id ?phase) (state PAUSED|FINISHED))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (if (pb-has-field ?p "grasp_notification")
   then
    (printout t "Benchmark feedback valid" crlf)

    (if (eq (pb-field-value ?p "grasp_notification") 1)
     then
      (assert (benchmark-feedback (source CLIENT) (time ?rcvd-at) (type SUCCESS)))
     else
      (assert (benchmark-feedback (source CLIENT) (time ?rcvd-at) (type FAIL)))
    )
   else
    (printout t "Benchmark feedback from " ?from-host ":" ?from-port " is invalid" crlf)
  )
)

; In case there is no rule consuming the feedback, retract it here
(defrule retract-benchmark-feedback
  (declare (salience ?*PRIORITY_LAST*))
  ?f <- (benchmark-feedback)
  =>
  (retract ?f)
)
