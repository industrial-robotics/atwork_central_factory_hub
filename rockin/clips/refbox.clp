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
(load* (resolve-file task-benchmarks.clp))
(load* (resolve-file functionality-benchmarks.clp))
(load* (resolve-file net.clp))
(load* (resolve-file robots.clp))

(defrule load-device-quality-control-camera
  (init)
  (have-feature QualityControlCamera)
  =>
  (printout t "Enabling Quality Control Camera" crlf)
  (load* (resolve-file device-quality-control-camera.clp))
)
