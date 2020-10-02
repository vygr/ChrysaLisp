;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Formes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")
(import "lib/yaml-data/yaml-xchange.lisp")

(defq
  +logging_srvc_name+ "LOG_SERVICE"
  log_mbox_id nil)

(structure '+log_event 0
  (byte
    'logmsg+
    'register+
    'shutdown+))

(defun-bind log-msg ()
  (defq msg (properties))
  msg)

(defun-bind log-registration (mname)
  (setp! (log-msg) :name mname t))

(defun-bind get-logsrvc-mb ()
  (cond
    ((nil? log_mbox_id)
      (defq res (mail-enquire +logging_srvc_name+))
      (when (nempty? res)
        (setq log_mbox_id (to-num (second (split (first res) ","))))))
    (t log_mbox_id)))

; For debugging only... remove before merging
(defun-bind start-log-service ()
  (setq log_mbox_id (open-child "apps/logger/app.lisp" kn_call_child)))

(defun-bind shutdown-log-service ()
  (mail-send (char +log_event_shutdown+ long_size) (get-logsrvc-mb))
  (setq log_mbox_id nil))

; Register myself with service
(defun-bind logger (mname)
  ; (logger name [configuration]) -> log structure for use in all calls
  ; Registers name with logging service and sends optional configuration
  ; information
  ; Part of the registering includes a return mbox that the service
  ; can write to
  (mail-send
    (cat
      (char +log_event_register+ long_size)
      (str (yaml-xser (log-registration mname))))
    (get-logsrvc-mb)))

(defun-bind log-trace (msg &rest kvpairs))
(defun-bind log-debug (msg &rest kvpairs))
(defun-bind log-info (msg &rest kvpairs)
  (mail-send (char +log_event_logmsg+ long_size) (get-logsrvc-mb)))
(defun-bind log-warning (msg &rest kvpairs))
(defun-bind log-critical (msg &rest kvpairs))

; (defq mbx (open-child "apps/logger/app.lisp" kn_call_child))
; (mail-send (const (char +log_event_close-service+ long_size)) (get-logsrvc-mb))
