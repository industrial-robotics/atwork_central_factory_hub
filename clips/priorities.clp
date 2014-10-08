;---------------------------------------------------------------------------
;  priorities.clp - RoCKIn RefBox CLIPS rule priorities
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defglobal
  ?*PRIORITY_FIRST*   =  5000
  ?*PRIORITY_HIGHER*  =  1000
  ?*PRIORITY_HIGH*    =   500
  ?*PRIORITY_CLEANUP* = -4000
  ?*PRIORITY_LAST*    = -5000
)
