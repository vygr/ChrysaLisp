;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/xchange/yaml-data.inc")
(import "apps/logger/loghandlers.lisp")
(import "apps/logger/logserver.lisp")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  (defq
    active    t
    sipc      (log-server (task-mailbox))
    debugwrt  (debug-logger)
    entry     (mail-declare
                (task-mailbox)
                +logging_srvc_name+
                "Logging Service 1.0"))

  (defmacro debug-write (&rest _)
    (when (def? 'debugwrt)
      `(. debugwrt :write :debug ~_)))

  ; Instantiate persistent loggers
  ; This is both statically defined in logsrvc.yaml and
  ; any registered loggers from previous seessions
  (load-loggers)

  (while active
    (bind '(client cmd msg) (. sipc :read_mail))
    (cond
      ; Ping event
      ((eql cmd :ping)
        (. sipc :client_ping client))
      ; Shutdown event
      ((eql cmd :shutdown)
        (setq active nil)
        (. sipc :server_shutdown client))
      ; Register event
      ((eql cmd :register)
       (cond
         ((eql (gets msg :kind) :anchor)
          (debug-write "Anchor registration for " (gets msg :name)))
         ((eql (gets msg :kind) :logger)
          (debug-write "Logging registration for " (gets msg :name)))
         (t
           (throw "Unknown registration " (tolist msg))))
       (. sipc :register_client client))
      ; Deregister event
      ((eql cmd :deregister)
       (. sipc :deregister_client client))
       )
    )
  (mail-forget entry))
