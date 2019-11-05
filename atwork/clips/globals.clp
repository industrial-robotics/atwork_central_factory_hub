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
  ?*BC-INVENTORY-PERIOD* = 1.0
  ?*BC-INVENTORY-BURST-PERIOD* = 0.5
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
  ?*BC-INVENTORY-BURST-COUNT* = 10

  ; How often and in what period should the version information
  ; be send over the network when a new peer is detected?
  ?*BC-VERSIONINFO-PERIOD* = 0.5
  ?*BC-VERSIONINFO-COUNT* = 10

  ; Benchmark times; seconds
  ?*BNT-PREPARATION-TIME*  =  180
  ?*BNT-EXECUTION-TIME*    =  300
  ?*BMT-PREPARATION-TIME*  =  180
  ?*BMT-EXECUTION-TIME*    =  300
  ?*BTT-PREPARATION-TIME*  =  180
  ?*BTT1-EXECUTION-TIME*   =  360
  ?*BTT2-EXECUTION-TIME*   =  600
  ?*BTT3-EXECUTION-TIME*   =  600
  ?*PPT-PREPARATION-TIME*  =  180
  ?*PPT-EXECUTION-TIME*    =  240
  ?*CBT-PREPARATION-TIME*  =  180
  ?*CBT-EXECUTION-TIME*    =  240
  ?*AWF-PREPARATION-TIME*  =  180
  ?*AWF-EXECUTION-TIME*    =  780
  ?*IRL-PREPARATION-TIME*  =  180
  ?*IRL-EXECUTION-TIME*    =  300

  ; Benchmark repetitions
  ?*BNT-COUNT*    =  1
  ?*BMT-COUNT*    =  1
  ?*BTT-COUNT*    =  1
  ?*PPT-COUNT*    =  1
  ?*CBT-COUNT*    =  1
  ?*AWF-COUNT*    =  1
  ?*IRL-COUNT*    =  1

  ?*ROBOCUP-OBJECTS* = (create$
    [F20_20_B] [F20_20_G] [S40_40_B] [S40_40_G] [M20_100] [M20] [M30] [R20]
  )
  ?*ROCKIN-OBJECTS* = (create$
    [BEARING_BOX] [BEARING] [AXIS] [DISTANCE_TUBE] [MOTOR]
  )

  ?*SHELF-LOCATIONS* = (create$
    [shelf-01] [shelf-02]
  )
  ?*WORKSTATION-0CM-LOCATIONS* = (create$
    [workstation-01] [workstation-07] [workstation-14] [workstation-11]
  )
  ?*WORKSTATION-5CM-LOCATIONS* = (create$
    [workstation-12] [workstation-08] 
  )
  ?*WORKSTATION-10CM-LOCATIONS* = (create$
    [workstation-02] [workstation-03] [workstation-05] [workstation-06] [workstation-15] [workstation-16] [workstation-13]
  )
  ?*WORKSTATION-15CM-LOCATIONS* = (create$
    [workstation-04] [workstation-09] [workstation-10] 
  )
  ?*PRECISION-LOCATIONS* = (create$
    [precision-01]
  )
  ?*ROTATING-TABLE-LOCATIONS* = (create$
    [conveyorbelt-01]

  )
  )
  ?*ROTATING-TABLE-LOCATIONS* = (create$ [conveyorbelt-02] )
  ?*CONVEYOR-BELT-LOCATIONS* = (create$ [conveyorbelt-01] )
  ?*WAYPOINT-LOCATIONS* = (create$
    [waypoint-01] [waypoint-02] [waypoint-03] [waypoint-04] [waypoint-05]
    [waypoint-06] [waypoint-07] [waypoint-08] [waypoint-09] [waypoint-10]
    [waypoint-11] [waypoint-12] [waypoint-13] [waypoint-14] [waypoint-15]
  )
)
