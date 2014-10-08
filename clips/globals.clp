;---------------------------------------------------------------------------
;  globals.clp - RoCKIn RefBox global CLIPS variables
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defglobal
  ; network sending periods; seconds
  ?*BEACON-PERIOD* = 1.0
  ?*BENCHMARKSTATE-PERIOD* = 0.1
  ?*BENCHMARKINFO-PERIOD* = 0.1
  ?*ROBOTINFO-PERIOD* = 0.25
  ?*INVENTORY-PERIOD* = 1.0
  ?*DRILLING-MACHINE-PERIOD* = 0.1
  ?*CONVEYOR-BELT-PERIOD* = 0.1
  ?*FORCE-FITTING-MACHINE-PERIOD* = 0.1
  ?*BC-ORDERINFO-PERIOD* = 2.0
  ?*BC-ORDERINFO-BURST-PERIOD* = 0.5

  ; This value is set by the rule config-timer-interval from config.yaml
  ?*TIMER-INTERVAL* = 0.0

  ; Time (sec) after which to warn about a robot lost
  ?*PEER-LOST-TIMEOUT* = 5
  ?*PEER-REMOVE-TIMEOUT* = 1080
  ?*PEER-TIME-DIFFERENCE-WARNING* = 3.0

  ; number of burst updates before falling back to slower updates
  ?*BC-ORDERINFO-BURST-COUNT* = 10

  ; How often and in what period should the version information
  ; be send over the network when a new peer is detected?
  ?*BC-VERSIONINFO-PERIOD* = 0.5
  ?*BC-VERSIONINFO-COUNT* = 10

  ; Benchmark times; seconds
  ?*TBM-TIME*               =  600
  ?*FBM1-TIME*              =  120  ; per run
  ?*FBM2-PREPARATION-TIME*  =   30
  ?*FBM2-EXECUTION-TIME*    =  210  ; per run
  ?*FBM3-CALIBRATION-TIME*  =   60
  ?*FBM3-PREPARATION-TIME*  =   60
  ?*FBM3-EXECUTION-TIME*    =  120  ; per run

  ; Benchmark repetitions
  ?*TBM-COUNT*    =  1
  ?*FBM1-COUNT*   = 10
  ?*FBM2-COUNT*   =  5
  ?*FBM3-COUNT*   =  5
)
