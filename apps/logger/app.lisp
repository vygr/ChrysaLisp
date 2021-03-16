;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")

(defq
  +logging_srvc  "LOG_SERVICE")

;single instance only
(when (= (length (mail-enquire +logging_srvc)) 0)

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
                +logging_srvc
                "Logging Service 1.0"))

  (defun debug-write (&rest msgs)
    ; (debug-write msg ...) -> nil
    (eval `(. debugwrt :write +logging_srvc :debug ~msgs)))

  (defun dbg-write (txt obj)
    (. debugwrt :write +logging_srvc :debug txt obj))

  (debug-write "Initialized server")

  ; Main processing loop

  (while active
    (bind '(client cmd msg) (. sipc :read_mail))
    (debug-write "Processing " cmd " registered? " (ipc? client))
    (cond
      ; Ping event
      ((eql cmd :ping)
       (. sipc :client_ping client))

      ; Shutdown event
      ((eql cmd :shutdown)
       (debug-write "Received 'shutdown' to service")
       (setq active nil)
       (. sipc :server_shutdown client))

      ; Query to see if :name has handler configured
      ((eql cmd :query_config)
       (debug-write "Received query_config for " (gets msg :name))
       ; Return handler if found else nil
       (. sipc
          :server_send
          client
          :query_result
          (xmap-kv :handler (config-for (gets msg :name)))))

      ; Add logger event
      ; Persists configuration and creates a client proxy
      ((eql cmd :add_handler)
       (defq
         lname  (gets msg :name)
         lkey   (gets msg :logger)
         lspec  (gets msg :logspec)
         lcfg   (gets-in lspec :logging :loggers lkey)
         hkey   (gets-in lcfg :handler)
         hcfg   (gets-in lspec :logging :handlers hkey))
       (debug-write
         "Received logspec add for " lname
         " logger " lkey
         " handler " hkey)
       (. sipc
          :server_send
          client
          :add_result
          (xmap-kv :handler (new-logger lname lkey lcfg hkey hcfg))))

      ; Register event
      ; Creates a client proxy that uses
      ; the logger identified in 'using'
      ((eql cmd :register)
       (debug-write
         "Received registration for " (gets msg :name)
         " using " (gets msg :using)
         " of kind " (gets msg :kind))
       (cond
         ; Anchor registering
         ((eql (gets msg :kind) :anchor)
          (defq lgr (logger-for (gets msg :using)))
          (. sipc :register_client
             client
             (gets msg :name )
             (get :name lgr)
             (get :handler lgr)))
         ; Regular client registering
         ((eql (gets msg :kind) :logger)
          (defq lgr (logger-for (gets msg :using)))
          (. sipc :register_client
             client
             (gets msg :name)
             (get :name lgr)
             (get :handler lgr)))
         (t
           (mail-forget entry)
           (throw "Unknown registration " (entries msg)))))

      ; Deregister event
      ((eql cmd :deregister)
       (. sipc :deregister_client client))

      ; Log messaging
      ((eql cmd :logmsg)
        (. client :log msg)))
    )
  (mail-forget entry))
