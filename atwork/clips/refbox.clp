;---------------------------------------------------------------------------
;  refbox.clp - RoCKIn RefBox CLIPS main file
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(load* (resolve-file location.clp))
(load* (resolve-file object-identifier.clp))
(load* (resolve-file item.clp))
(load* (resolve-file inventory.clp))
(load* (resolve-file order.clp))
(load* (resolve-file order-info.clp))
(load* (resolve-file benchmark-time.clp))
(load* (resolve-file state-machine.clp))
(load* (resolve-file benchmark.clp))
(load* (resolve-file basic-navigation-tests.clp))
;(load* (resolve-file basic-manipulation-tests.clp))
;(load* (resolve-file basic-transporation-tests.clp))
;(load* (resolve-file precision-placement-tests.clp))
;(load* (resolve-file conveyor-belt-tests.clp))
;(load* (resolve-file at-work-finals.clp))
;(load* (resolve-file industrial-robotics-league-challenges.clp))
(load* (resolve-file net.clp))
(load* (resolve-file robots.clp))

(defrule load-conveyor-belt
  (init)
  (have-feature ConveyorBelt)
  =>
  (printout t "Enabling Conveyor Belt" crlf)
  (load* (resolve-file device-conveyor-belt.clp))
)

(defrule load-drilling-machine
  (init)
  (have-feature DrillingMachine)
  =>
  (printout t "Enabling Drilling Machine" crlf)
  (load* (resolve-file device-drilling-machine.clp))
)

(defrule load-force-fitting-machine
  (init)
  (have-feature ForceFittingMachine)
  =>
  (printout t "Enabling Force Fitting Machine" crlf)
  (load* (resolve-file device-force-fitting-machine.clp))
)

(defrule load-device-quality-control-camera
  (init)
  (have-feature QualityControlCamera)
  =>
  (printout t "Enabling Quality Control Camera" crlf)
  (load* (resolve-file device-quality-control-camera.clp))
)
