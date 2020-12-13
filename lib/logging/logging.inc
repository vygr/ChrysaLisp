;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Library for clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/logging/logcommons.inc")

(defun console-log-msg-writer (logger msgd)
  ; (console-log-msg-writer logconfig mail-message) -> stream
  (log-write stdout (str
               " ["(log-level-string logger (gets msgd :msg-level))"] "
               (gets logger :name)": ") (gets msgd :message)))


(defun log-registration-result (logger)
  ; (log-registration-result xmap) -> xmap
  (defq
    msg   (mail-read (gets logger :reciever))
    id    (get-long msg mail_msg_command))
  (cond
    ; Succesful registration
    ((= id +log_event_registered+)
      (merges logger (deser-inbound msg)))
    (t
      (throw "Invalid registration " msg))))

(defun create-logger (mname &optional uses_handler)
  ; (create-logger name [kw]) -> logger
  ; Registers name with logging service and sends optional logger
  ; service to use:
  ;   :log_anchor (default)
  ;   :console
  ;   :file
  ; Part of the registering includes a return mbox that the service
  ; can write to
  (defq ankw (get '*log_anchor* (penv (env))))
  (when (not uses_handler)
    (cond
      (ankw (setd uses_handler ankw))
      (t (setd uses_handler :console))))
  (defq lr (log-registration mname uses_handler))
  (mail-send
    (cat
      (char +log_event_register+ long_size)
      (str (data-xser lr)))
    (get-logsrvc-mb))
  (log-registration-result lr))

(defun log-send (logger lmsgtype msg)
  ; (log-send logger msgtype msg)
  ; Generic send log message to service unless
  ; the service is console
  (defq msgs (xmap-kv
          :module     (gets logger :token)
          :msg-level  lmsgtype
          :message    (str msg)))
  (cond
    ((eql (gets logger :handler) :console_handler)
      (defq
        lvli  (log-level-index logger lmsgtype)
        lvlc  (log-level-index logger (log-level-base logger)))
      (if (>= lvli lvlc)
        (console-log-msg-writer logger msgs)))
    (t
      (mail-send
        (cat
          (char +log_event_logmsg+ long_size)
          (str (data-xser msgs)))
        (get-logsrvc-mb)))))

; Convenience loggers

(defun log-debug (logger msg) (log-send logger :debug msg))
(defun log-info (logger msg) (log-send logger :info msg))
(defun log-warning (logger msg) (log-send logger :warning msg))
(defun log-error (logger msg) (log-send logger :error msg))
(defun log-critical (logger msg) (log-send logger :critical msg))

