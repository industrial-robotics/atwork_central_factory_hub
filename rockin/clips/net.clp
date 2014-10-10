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

  (if (and (eq ?team "LLSF") (eq ?name "RefBox"))
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
