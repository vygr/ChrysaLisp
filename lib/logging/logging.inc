;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Library for clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")
(import "lib/yaml-data/yaml-xchange.lisp")
(import "lib/date/date.inc")

; Setup timezone for now
(timezone-init "America/New_York")

(defq
  +logging_srvc_name+ "LOG_SERVICE"
  log_mbox_id nil)

; Mail message structure
; command - See +log_event+ below
; data - String block of data (yaml-xser'd)
(structure 'mail_msg 0
  (long 'command)
  (offset 'data))

; Events for mail_msg_commands
(structure '+log_event 0
  (byte
    'logmsg+
    'register+
    'deregister+
    'shutdown+
    'registered+
    'errored+))

(defun-bind deser-inbound (msg)
  ; (deser-inbound mail-msg) -> properties
  ; Deserializes inbound data from mail message
  (yaml-xdeser (write (string-stream (cat "")) (slice mail_msg_data -1 msg))))

(defun-bind log-write (sstrm &rest _)
  ; (log-write strean ....) -> stream
  ; Wrap timestamp and nl to '_' arguments
  (setq _ (insert (push _ +nl+) 0 (list (encode-date (date)))))
  (write sstrm (apply str _))
  (stream-flush sstrm))

(defun-bind console-log-msg-writer (logger msgd)
  ; (console-log-msg-writer logconfig mail-message) -> stream
  (log-write stdout (str
               " [" (getp msgd :msg-level)"] "
               (getp logger :name)": ") (getp msgd :message)))


(defun-bind log-registration (mname configuration)
  ; (log-registration name properties) -> properties
  ; Setups the logging registration and optional configuration
  (defq p (setsp! (properties)
    :name     mname
    :reciever (mail-alloc-mbox)))
  (if configuration
    (setp! p :configuration configuration)
    p))

(defun-bind get-logsrvc-mb ()
  ; (get-logsrvc-mb) -> mailbox id | nil
  (cond
    ((nil? log_mbox_id)
      (defq res (mail-enquire +logging_srvc_name+))
      (when (nempty? res)
        (setq log_mbox_id (to-num (second (split (first res) ","))))))
    (t log_mbox_id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For debugging only... remove before merging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-bind start-log-service ()
  (setq log_mbox_id (open-child "apps/logger/app.lisp" kn_call_child)))

(defun-bind shutdown-log-service ()
  (mail-send (char +log_event_shutdown+ long_size) (get-logsrvc-mb))
  (setq log_mbox_id nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-bind logresult (logger)
  ; (logresult properties) -> properties
  (defq
    msg   (mail-read (getp logger :reciever))
    id    (get-long msg mail_msg_command))
  (cond
    ; Succesful registration
    ((= id +log_event_registered+)
      (pmerge logger (deser-inbound msg)))
    (t
      (throw "Invalid registration " msg))))

(defun-bind create-logger (mname &optional configuration)
  ; (create-logger name [configuration]) -> properties
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

(defun-bind log-send (logger lmsgtype msg)
  ; (log-send logger msgtype msg)
  ; Generic send log message to service unless
  ; the service is console
  (defq msgs (properties
          :module     (getp logger :token)
          :msg-level  lmsgtype
          :message    (str msg)))
  (cond
    ((eql (getp logger :logger) :console)
      (defq
        llvls (getp logger :levels)
        lvli  (getp llvls lmsgtype)
        lvlc  (getp llvls (getp-in logger :configuration :level)))
      (if (>= lvli lvlc)
        (console-log-msg-writer logger msgs)))
    (t
      (mail-send
        (cat
          (char +log_event_logmsg+ long_size)
          (str (yaml-xser msgs)))
        (get-logsrvc-mb)))))

; Convenience loggers

(defun-bind log-debug (logger msg) (log-send logger :debug msg))
(defun-bind log-info (logger msg) (log-send logger :info msg))
(defun-bind log-warning (logger msg) (log-send logger :warning msg))
(defun-bind log-error (logger msg) (log-send logger :error msg))
(defun-bind log-critical (logger msg) (log-send logger :critical msg))

