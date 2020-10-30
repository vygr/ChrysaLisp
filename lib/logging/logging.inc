;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logging - Logging Service Library for clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/logging/logcommons.inc")

(defun-bind console-log-msg-writer (logger msgd)
  ; (console-log-msg-writer logconfig mail-message) -> stream
  (log-write stdout (str
               " ["(log-level-string logger (gets msgd :msg-level))"] "
               (gets logger :name)": ") (gets msgd :message)))


(defun-bind log-registration-result (logger)
  ; (log-registration-result properties) -> properties
  (defq
    msg   (mail-read (gets logger :reciever))
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
  (print "using " uses_handler)
  (defq lr (log-registration mname uses_handler))
  (mail-send
    (cat
      (char +log_event_register+ long_size)
      (str (yaml-xser lr)))
    (get-logsrvc-mb))
  (log-registration-result lr))

(defun-bind log-send (logger lmsgtype msg)
  ; (log-send logger msgtype msg)
  ; Generic send log message to service unless
  ; the service is console
  (defq msgs (properties
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
          (str (yaml-xser msgs)))
        (get-logsrvc-mb)))))

; Convenience loggers

(defun-bind log-debug (logger msg) (log-send logger :debug msg))
(defun-bind log-info (logger msg) (log-send logger :info msg))
(defun-bind log-warning (logger msg) (log-send logger :warning msg))
(defun-bind log-error (logger msg) (log-send logger :error msg))
(defun-bind log-critical (logger msg) (log-send logger :critical msg))

