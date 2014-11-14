;---------------------------------------------------------------------------
;  globals.clp - RoCKIn RefBox global CLIPS variables
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defglobal
  ; network sending periods; seconds
  ?*BEACON-PERIOD* = 1.0
  ?*BENCHMARKSTATE-PERIOD* = 1.0
  ?*INVENTORY-PERIOD* = 1.0
  ?*DRILLING-MACHINE-PERIOD* = 0.1

  ; This value is set by the rule config-timer-interval from config.yaml
  ?*TIMER-INTERVAL* = 0.0

  ; Time (sec) after which to warn about a robot lost
  ?*PEER-LOST-TIMEOUT* = 5
  ?*PEER-REMOVE-TIMEOUT* = 1080
  ?*PEER-TIME-DIFFERENCE-WARNING* = 3.0

  ; How often and in what period should the version information
  ; be send over the network when a new peer is detected?
  ?*BC-VERSIONINFO-PERIOD* = 0.5
  ?*BC-VERSIONINFO-COUNT* = 10
)
