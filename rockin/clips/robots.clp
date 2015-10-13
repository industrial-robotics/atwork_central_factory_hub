;---------------------------------------------------------------------------
;  robots.clp - RoCKIn RefBox CLIPS - keeping tracks of robots in game
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

; Determine the order of two robots. If the robots are in the same team, sort
; them by the robot name, else sort them by the team name.
(deffunction robot-order (?r1 ?r2)
  (if (eq (fact-slot-value ?r1 team) (fact-slot-value ?r2 team))
   then
    (string-gt (fact-slot-value ?r2 name) (fact-slot-value ?r1 name))
   else
    (string-gt (fact-slot-value ?r2 team) (fact-slot-value ?r1 team))
  )
)

(defrule robot-lost
  (time $?now)
  ?rf <- (robot (team ?team) (name ?name) (host ?host) (port ?port)
    (warning-sent FALSE) (last-seen $?ls&:(timeout ?now ?ls ?*PEER-LOST-TIMEOUT*)))
  =>
  (modify ?rf (warning-sent TRUE))
  (printout warn "Robot " ?name "/" ?team " at " ?host " lost" crlf)
  (assert (attention-message (team ?team)
           (text (str-cat "Robot " ?name "/" ?team
              " at " ?host " lost"))))
)

(defrule robot-remove
  (time $?now)
  ?rf <- (robot (team ?team) (name ?name) (host ?host) (port ?port)
    (last-seen $?ls&:(timeout ?now ?ls ?*PEER-REMOVE-TIMEOUT*)))
  =>
  (retract ?rf)
  (printout warn "Robot " ?name "/" ?team " at " ?host " definitely lost" crlf)
  (assert
   (attention-message (text (str-cat "Robot " ?name "/" ?team
             " at " ?host " definitely lost"))))
)
