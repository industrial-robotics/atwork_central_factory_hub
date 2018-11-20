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
  ?*BNT-PREPARATION-TIME*  =  300
  ?*BNT-EXECUTION-TIME*    =  300
  ?*BMT-PREPARATION-TIME*  =  300
  ?*BMT-EXECUTION-TIME*    =  300
  ?*BTT-PREPARATION-TIME*  =  300
  ?*BTT-EXECUTION-TIME*    =  300
  ?*PPT-PREPARATION-TIME*  =  300
  ?*PPT-EXECUTION-TIME*    =  300
  ?*CBT-PREPARATION-TIME*  =  300
  ?*CBT-EXECUTION-TIME*    =  300
  ?*AWF-PREPARATION-TIME*  =  300
  ?*AWF-EXECUTION-TIME*    =  600
  ?*IRL-PREPARATION-TIME*  =  300
  ?*IRL-EXECUTION-TIME*    =  300
  ; ERL benchmark times;
  ?*TBM-TIME*               =  600
  ?*FBM1-TIME*              =  120  ; per run
  ?*FBM2-PREPARATION-TIME*  =   30
  ?*FBM2-EXECUTION-TIME*    =  210  ; per run
  ?*FBM3-CALIBRATION-TIME*  =   60
  ?*FBM3-PREPARATION-TIME*  =   60
  ?*FBM3-EXECUTION-TIME*    =  120  ; per run


  ; Benchmark repetitions
  ?*BNT-COUNT*    =  1
  ?*BMT-COUNT*    =  1
  ?*BTT-COUNT*    =  1
  ?*PPT-COUNT*    =  1
  ?*CBT-COUNT*    =  1
  ?*AWF-COUNT*    =  1
  ?*IRL-COUNT*    =  1
  ?*TBM-COUNT*    =  1
  ?*FBM1-COUNT*   = 10
  ?*FBM2-COUNT*   =  5
  ?*FBM3-COUNT*   =  5

  ?*ROBOCUP-OBJECTS* = (create$
    [F20_20_B] [F20_20_G] [S40_40_B] [S40_40_G] [M20_100] [M20] [M30] [R20]
  )
  ?*ROCKIN-OBJECTS* = (create$
    [BEARING_BOX] [BEARING] [AXIS] [DISTANCE_TUBE] [MOTOR]
  )
  ?*CHOCOLATE-OBJECTS* = (create$
    [PICKUP_BLACK_WHITE] [PICKUP_CHOCO_MILK] [DUPLO_WHITE] [DUPLO_CLASSIC]
    [TWIX_SPEKULATIUS] [TWIX_WHITE] [TWIX_CLASSIC] [TWIX_MINI] [MARS] [MARS_MINI]
    [SNICKERS] [SNICKERS_MINI] [KITKAT_CLASSIC] [KITKAT_WHITE] [KITKAT_CHUNKY_WHITE]
    [KITKAT_CHUNKY_CLASSIC] [KITKAT_MINI] [LION_CLASSIC] [LION_MINI] [M_M_CRIPSY]
    [M_M_PEANUT] [BOUNTY] [BOUNTY_MINI] [MILKYWAY] [MILKYWAY_MINI]
  )
  ?*RITTERSPORT-OBJECTS* = (create$
    [RITTERSPORT_KNUSPERKEKS] [RITTERSPORT_JOGHURT] [RITTERSPORT_KNUSPERFLAKES]
    [RITTERSPORT_NUSS_SPLITTER] [RITTERSPORT_MARZIPAN] [RITTERSPORT_NUGAT]
  )
  ?*SHELF-LOCATIONS* = (create$
    [shelf-01] [shelf-02]
  )
  ?*WORKSTATION-0CM-LOCATIONS* = (create$
    [workstation-01] [workstation-07] [workstation-11]
  )
  ?*WORKSTATION-5CM-LOCATIONS* = (create$
    [workstation-12] [workstation-08]
  )
  ?*WORKSTATION-10CM-LOCATIONS* = (create$
    [workstation-02] [workstation-03] [workstation-05] [workstation-06]
  )
  ?*WORKSTATION-15CM-LOCATIONS* = (create$
    [workstation-04] [workstation-09] [workstation-10]
  )
  ?*PRECISION-LOCATIONS* = (create$
    [precision-01]
  )
  ?*CONVEYOR-LOCATIONS* = (create$
    [conveyorbelt-01] [conveyorbelt-02]
  )
  ?*ROTATING-TABLE-LOCATIONS* = (create$ [conveyorbelt-02] )
  ?*CONVEYOR-BELT-LOCATIONS* = (create$ [conveyorbelt-01] )
  ?*WAYPOINT-LOCATIONS* = (create$
    [waypoint-01] [waypoint-02] [waypoint-03] [waypoint-04] [waypoint-05]
    [waypoint-06] [waypoint-07] [waypoint-08] [waypoint-09] [waypoint-10]
    [waypoint-11] [waypoint-12] [waypoint-13] [waypoint-14] [waypoint-15]
  )
)
