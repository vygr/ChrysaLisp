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
    'shutdown+
    'registered+
    'errored+))

(defun-bind log-msg ()
  (defq msg (properties))
  msg)

(defun-bind log-registration (mname configuration)
  (defq p (setsp! (log-msg)
    :name     mname
    :reciever (mail-alloc-mbox)))
  (if configuration
    (setp! p :configuration configuration)
    p))

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

; Log receipt handler
(defun-bind logresult (logger)
  ; (logresult properties) -> properties
  logger)

(defun-bind reg-logger (mname &optional configuration)
  ; (logger name [configuration]) -> properties
  ; Registers name with logging service and sends optional configuration
  ; information
  ; Part of the registering includes a return mbox that the service
  ; can write to
  (defq lr (log-registration mname configuration))
  (mail-send
    (cat
      (char +log_event_register+ long_size)
      (str (yaml-xser lr)))
    (get-logsrvc-mb))
  (logresult lr))

(defun-bind dereg-logger (logger)

  (logresult lr))

(defun-bind log-trace (logger msg &rest kvpairs))
(defun-bind log-debug (logger msg &rest kvpairs))
(defun-bind log-info (logger msg &rest kvpairs)
  (mail-send (char +log_event_logmsg+ long_size) (get-logsrvc-mb)))
(defun-bind log-warning (logger msg &rest kvpairs))
(defun-bind log-critical (logger msg &rest kvpairs))

; (defq mbx (open-child "apps/logger/app.lisp" kn_call_child))
; (mail-send (const (char +log_event_close-service+ long_size)) (get-logsrvc-mb))
