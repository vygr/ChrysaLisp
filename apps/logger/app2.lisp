;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)

  (import "lib/xchange/yaml-data.inc")
  (import "apps/logger/loghandlers.lisp")
  (import "apps/logger/logserver.lisp")

  ; Instantiate persistent loggers
  ; These are initially defined in logsrvc.yaml,
  ; persisted to the log directory and then modified
  ; over time

  (defq
    active    t
    sipc      (log-server (task-mailbox))
    debugwrt  (load-loggers t)
    entry     (mail-declare
                (task-mailbox)
                +logging_srvc_name+
                "Logging Service 1.0"))

  (defun debug-write (&rest msgs)
    ; (debug-write msg ...) -> nil
    (eval `(. debugwrt :write +logging_srvc_name+ :debug ~msgs)))

  (debug-write "Initialized server")

  ; Main processing loop

  (while active
    (bind '(client cmd msg) (. sipc :read_mail))
    (cond
      ; Ping event
      ((eql cmd :ping)
       (debug-write "Received 'ping' to service")
       (. sipc :client_ping client))
      ; Shutdown event
      ((eql cmd :shutdown)
       (debug-write "Received 'shutdown' to service")
       (setq active nil)
       (. sipc :server_shutdown client))
      ; Register event
      ((eql cmd :register)
       (cond
         ((eql (gets msg :kind) :anchor)
          (debug-write "Anchor registration for " (gets msg :name)))
         ((eql (gets msg :kind) :logger)
          (debug-write "Logging registration for " (gets msg :name))
          (. sipc :register_client client))
         (t
           (debug-write "Unknown registration " (entries msg))
           (mail-forget entry)
           (throw "Unknown registration " (entries msg)))))
      ; Deregister event
      ((eql cmd :deregister)
       (. sipc :deregister_client client)))
    )
  (mail-forget entry))
