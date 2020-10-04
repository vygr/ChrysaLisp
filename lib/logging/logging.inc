;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Library for clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")
(import "lib/yaml-data/yaml-xchange.lisp")

(defq
  +logging_srvc_name+ "LOG_SERVICE"
  log_mbox_id nil)

; Mail message structure
; command - See +log_event+
; data - String block of data (yaml-xser'd)
(structure 'mail_msg 0
  (long 'command)
  (offset 'data))

; (structure 'mail_msg 0
;   (long 'command)
;   (offset 'data))

; Events for mail_msg_commands
(structure '+log_event 0
  (byte
    'logmsg+
    'register+
    'deregister+
    'shutdown+
    'registered+
    'errored+))

; Log message types
(structure '+log_message 0
  (byte
    'debug+
    'info+
    'warning+
    'error+
    'critical+))

(defun-bind log-registration (mname configuration)
  (defq p (setsp! (properties)
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
  (defq
    msg   (mail-read (getp logger :reciever))
    id    (get-long msg mail_msg_command)
    data  (slice mail_msg_data -1 msg))
  (cond
    ; Succesful registration
    ((= id +log_event_registered+)
      (pmerge logger (yaml-xdeser (write (string-stream (cat "")) data))))
    (t
      (throw "Invalid registration " msg))))

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
      (str (yaml-xser (properties
        :module   (getp logger :token)
        :msg-type lmsgtype
        :message  (str msg)))))
    (get-logsrvc-mb)))

(defun-bind log-debug (logger msg) (log-send logger +log_message_debug+ msg))
(defun-bind log-info (logger msg) (log-send logger +log_message_info+ msg))
(defun-bind log-warning (logger msg) (log-send logger +log_message_warning+ msg))
(defun-bind log-error (logger msg) (log-send logger +log_message_error+ msg))
(defun-bind log-critical (logger msg) (log-send logger +log_message_critical+ msg))

; (defq mbx (open-child "apps/logger/app.lisp" kn_call_child))
; (mail-send (const (char +log_event_close-service+ long_size)) (get-logsrvc-mb))
