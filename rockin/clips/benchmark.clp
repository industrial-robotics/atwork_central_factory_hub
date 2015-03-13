;---------------------------------------------------------------------------
;  benchmark.clp - LLSF RefBox CLIPS benchmark maintenance
;
;  Created: Tue Jun 11 15:19:25 2013
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deffunction select-random-object ()
  (bind ?objects (create$))
  (do-for-all-facts ((?o benchmark-object)) TRUE
    (bind ?objects (insert$ ?objects 1 ?o))
  )

  (bind ?selected-object (pick-random$ ?objects))
  (bind ?benchmark-id (fact-slot-value ?selected-object benchmark-id))
  (bind ?selected-object-id (fact-slot-value ?selected-object object-id))

  (do-for-fact ((?o object-identifier)) (eq ?o:id ?selected-object-id)
    (bind ?description (nth$ 1 (fact-slot-value ?o description)))

    (printout t "FBM: Place object " ?description " (" ?benchmark-id ") in front "
        "of the robot and continue the benchmark" crlf)
    (assert (attention-message (text (str-cat "FBM: The robot should handle the "
        "object " ?description " (" ?benchmark-id ")"))))

    ; Make the selected object available e.g. for logging
    (assert (selected-object (object-id ?o:id)))
  )
)
