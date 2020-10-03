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
    'deregister+
    'shutdown+
    'registered+
    'errored+))

(structure '+log_message 0
  (byte
    'debug+
    'info+
    'warning+
    'critical+))

(defun-bind logger-inst ()
  (defq msg (properties))
  msg)

(defun-bind log-registration (mname configuration)
  (defq p (setsp! (logger-inst)
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

(defun-bind log-send (logger lmsgtype msg)
  (mail-send
    (cat
      (char +log_event_logmsg+ long_size)
      (char lmsgtype long_size)
      msg)
    (get-logsrvc-mb)))

(defun-bind log-debug (logger msg) (log-send logger +log_message_debug+ msg))
(defun-bind log-info (logger msg) (log-send logger +log_message_info+ msg))
(defun-bind log-warning (logger msg) (log-send logger +log_message_warning+ msg))
(defun-bind log-critical (logger msg) (log-send logger +log_message_critical+ msg))

; (defq mbx (open-child "apps/logger/app.lisp" kn_call_child))
; (mail-send (const (char +log_event_close-service+ long_size)) (get-logsrvc-mb))
