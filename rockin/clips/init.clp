;---------------------------------------------------------------------------
;  init.clp - RoCKIn RefBox CLIPS initialization file
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------


(defglobal
  ?*CLIPS_DIRS* = (get-clips-dirs)
  ?*DEBUG* = 2  ;debug levels: 0 ~ none, 1 ~ minimal, 2 ~ more, 3 ~ maximum
  ?*CONFIG_PREFIXES* = (create$ "/llsfrb")
  ?*START-TIME* = (now)
)


(deffunction resolve-file (?file)
  (foreach ?d ?*CLIPS_DIRS*
     (bind ?fn (str-cat ?d ?file))
     (if (open ?fn file-clips-tmp)
      then
       (close file-clips-tmp)
       (return ?fn)
     )
  )
  (return FALSE)
)

(load* (resolve-file utils.clp))
(load* (resolve-file time.clp))
(load* (resolve-file config.clp))
(load* (resolve-file priorities.clp))

(defrule load-config
  (init)
  =>
  (foreach ?p ?*CONFIG_PREFIXES*
    (load-config ?p)
  )
  (assert (config-loaded))
)

(defrule enable-debug
  (init)
  (confval (path "/llsfrb/clips/debug") (type BOOL) (value ?v))
  =>
  (if (eq ?v true) then
    (printout t "CLIPS debugging enabled, watching facts and rules" crlf)
    (watch facts)
    (watch rules)
  )
)

(defrule debug-level
  (init)
  (confval (path "/llsfrb/clips/debug-level") (type UINT) (value ?v))
  =>
  (printout t "Setting debug level to " ?v " (was " ?*DEBUG* ")" crlf)
  (bind ?*DEBUG* ?v)
)

(defrule silence-debug-facts
  (declare (salience -1000))
  (init)
  (confval (path "/llsfrb/clips/debug") (type BOOL) (value true))
  (confval (path "/llsfrb/clips/unwatch-facts") (type STRING) (is-list TRUE) (list-value $?lv))
  =>
  (printout t "Disabling watching of the following facts: " ?lv crlf)
  (foreach ?v ?lv (unwatch facts (sym-cat ?v)))
)

(defrule silence-debug-rules
  (declare (salience -1000))
  (init)
  (confval (path "/llsfrb/clips/debug") (type BOOL) (value true))
  (confval (path "/llsfrb/clips/unwatch-rules") (type STRING) (is-list TRUE) (list-value $?lv))
  =>
  (printout t "Disabling watching of the following rules: " ?lv crlf)
  (foreach ?v ?lv (unwatch rules (sym-cat ?v)))
)

(defrule announce-loading-done
  (declare (salience ?*PRIORITY_LAST*))
  (init)
  =>
  (printout t "RefBox loaded and ready to run" crlf)
)

(reset)
(seed (integer (time)))
