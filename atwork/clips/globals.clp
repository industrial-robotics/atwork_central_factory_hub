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
  ?*BC-TASKINFO-PERIOD* = 2.0
  ?*BC-TASKINFO-BURST-PERIOD* = 0.5

  ; This value is set by the rule config-timer-interval from config.yaml
  ?*TIMER-INTERVAL* = 0.0

  ; Time (sec) after which to warn about a robot lost
  ?*PEER-LOST-TIMEOUT* = 5
  ?*PEER-REMOVE-TIMEOUT* = 1080
  ?*PEER-TIME-DIFFERENCE-WARNING* = 3.0

  ; number of burst updates before falling back to slower updates
  ?*BC-TASKINFO-BURST-COUNT* = 10

  ; How often and in what period should the version information
  ; be send over the network when a new peer is detected?
  ?*BC-VERSIONINFO-PERIOD* = 0.5
  ?*BC-VERSIONINFO-COUNT* = 10

  ; Benchmark times; seconds
  ?*BNT-EXECUTION-TIME*    =  300
  ?*BNT-PREPARATION-TIME*  =  300
  ?*BMT-EXECUTION-TIME*    =  300
  ?*BMT-PREPARATION-TIME*  =  300
  ?*BTT-EXECUTION-TIME*    =  300
  ?*BTT-PREPARATION-TIME*  =  300
  ?*PPT-EXECUTION-TIME*    =  300
  ?*PPT-PREPARATION-TIME*  =  300
  ?*CBT-EXECUTION-TIME*    =  300
  ?*CBT-PREPARATION-TIME*  =  300
  ?*AWF-EXECUTION-TIME*    =  300
  ?*AWF-PREPARATION-TIME*  =  300
  ?*IRL-EXECUTION-TIME*    =  300
  ?*IRL-PREPARATION-TIME*  =  300

  ; Benchmark repetitions
  ?*BNT-COUNT*    =  1
  ?*BMT-COUNT*    =  1
  ?*BTT-COUNT*    =  1
  ?*PPT-COUNT*    =  1
  ?*CBT-COUNT*    =  1
  ?*AWF-COUNT*    =  1
  ?*IRL-COUNT*    =  1
)
