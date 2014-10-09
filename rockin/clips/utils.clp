;---------------------------------------------------------------------------
;  utils.clp - RoCKIn RefBox CLIPS utility functions
;
;  Copyright  2012  Tim Niemueller [www.niemueller.de]
;  Licensed under GPLv2+ license, cf. LICENSE file
;---------------------------------------------------------------------------

(deffunction debug (?level)
  (return (<= ?level ?*DEBUG*))
)

(deffunction append$ (?list $?items)
  (insert$ ?list (+ (length$ ?list) 1) ?items)
)

(deffunction randomize$ (?list)
  (bind ?l (length$ ?list))
  (loop-for-count 200 do
    (bind ?a (random 1 ?l))
    (bind ?b (random 1 ?l))
    (bind ?tmp (nth$ ?a ?list))
    (bind ?list (replace$ ?list ?a ?a (nth$ ?b ?list)))
    (bind ?list (replace$ ?list ?b ?b ?tmp))
  )
  (return ?list)
)

(deffunction pick-random$ (?list)
  (return (nth$ (random 1 (length$ ?list)) ?list)) 
)

(deffunction is-even-int (?num)
  (return (eq (mod ?num 2) 0))
)

(deffunction is-odd-int (?num)
  (return (eq (mod ?num 2) 1))
)

(deffunction string-gt (?s1 ?s2)
  (return (> (str-compare ?s1 ?s2) 0))
)
