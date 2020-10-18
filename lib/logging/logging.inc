;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Library for clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/logging/logcommons.inc")

(defq log_mbox_id nil)

(defun-bind console-log-msg-writer (logger msgd)
  ; (console-log-msg-writer logconfig mail-message) -> stream
  (log-write stdout (str
               " ["(log-level-string logger (getp msgd :msg-level))"] "
               (getp logger :name)": ") (getp msgd :message)))


(defun-bind log-registration (mname use_handler)
  ; (log-registration name keyword) -> properties
  ; Setups the logging registration and optional configuration
  (setsp! (properties)
    :name       mname
    :handler    use_handler
    :reciever   (mail-alloc-mbox)))

(defun-bind get-logsrvc-mb ()
  ; (get-logsrvc-mb) -> mailbox id | nil
  (cond
    ((nil? log_mbox_id)
      (defq res (mail-enquire +logging_srvc_name+))
      (setq log_mbox_id
      (if (nempty? res)
        (to-num (second (split (first res) ",")))
        (open-child "apps/logger/app.lisp" kn_call_child))))
    (t log_mbox_id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For debugging only... remove before merging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun-bind create-logger (mname &optional uses_handler)
  ; (create-logger name [kw]) -> properties
  ; Registers name with logging service and sends optional logger
  ; service to use:
  ;   :console (default)
  ;   :file
  ; Part of the registering includes a return mbox that the service
  ; can write to
  (setd uses_handler :console)
  (defq lr (log-registration mname uses_handler))
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
    ((eql (getp logger :handler) :console_handler)
      (defq
        lvli  (log-level-index logger lmsgtype)
        lvlc  (log-level-index logger (log-level-base logger)))
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

