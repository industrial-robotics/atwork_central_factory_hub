;---------------------------------------------------------------------------
;  facts.clp - RoCKIn RefBox CLIPS - facts specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deftemplate network-client
  (slot id (type INTEGER))
  (slot host (type STRING))
  (slot port (type INTEGER))
)
