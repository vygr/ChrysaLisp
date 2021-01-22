;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logservice - Logging client IPC library
; Log Service - a flexible log event
; system for commands, applicaations and
; libraries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/logging/logservice.inc")
(import "lib/date/date.inc")

(defq clog nil)

; log-console
(defclass log-console (level) (log-client)
  (def this
    :level level)

  (defmethod :change_level (this level)
    (set this :level level))

  (defmethod :send_log_msg (this loglevel msg)
    (prin (apply str
           (insert
             (push msg +nl+)
             0
             (list (encode-date) " [" loglevel "] ")))))

  (defmethod :shutdown (this)
    (. this :send_log_msg :error "Shutdown requires non-console logger"))

  (defmethod :ping (this)
    (. this :send_log_msg :error "Ping requires non-console logger"))
  )

(when (nil? clog)
  (setq clog (log-console :info)))
