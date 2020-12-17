;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logservice.inc")
(import "lib/logging/logcommons.inc")
(import "lib/xchange/yaml-data.inc")
(import "apps/logger/logutils.lisp")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  ; Setup general purpose informationnet_id_s
  (defq
    registra  (xmap)
    active    t
    entry (mail-declare (task-mailbox) +logging_srvc_name+ "Logging Service 0.5")
    DEBUG (file-stream "./logs/DEBUG_SERVICE.log" file_open_append)
    )

  (defun debug-write (&rest _)
    (write DEBUG (apply str (push _ +nl+)))
    (stream-flush DEBUG))
  ; (defun debug-write (&rest _))

  ; Process configuration files
  (bind '(srvc_fh fcfg? conf fmap registry) (process-log-cfg))

  (log-write (gets srvc_fh :handle) " Starting LOG_SERVICE")
  (debug-write "Log Service Started")

  (defun log-msg-writer (msg)
    ; (log-msg-writer mail-message) -> stream
    (defq
      msgd (deser-inbound msg)
      cnfg (gets registra (gets msgd :module))
      hndl (loghandler fmap cnfg)
      mlvl (gets msgd :msg-level))
    (when (>= (log-level-index cnfg mlvl)
              (log-level-index cnfg (gets hndl :level)))
      (log-write (gets hndl :handle) (str
                 " ["(log-level-string cnfg mlvl)"] "
                 (gets cnfg :name)": ") (gets msgd :message))))


  (defun register-logger (config)
    ; (register-logger properties) -> ?

    (defq chsh (hash config))
    ; Basics
    (sets-pairs! config
      :log_lvl :info
      :token chsh
      :levels (gets-in conf :logging :levels))
    ; Resolve handler
    (case (gets config :handler)
      ; Keyword cases
      ((:console)
       (defq hndl (gets-in conf :logging :loggers :console :handler))
       (sets-pairs! config
          :log_lvl (gets-in conf :logging :handlers hndl :level)
          :handler hndl))
      ((:file)
       (defq hndl (gets-in conf :logging :loggers :file :handler))
       (sets-pairs! config
          :log_lvl (gets (gets fmap hndl) :level)
          :handler hndl))
      ((:system)
       (defq hndl (gets-in conf :logging :loggers :system :handler))
       (sets-pairs! config
          :log_lvl (gets (gets fmap hndl):level)
          :handler hndl))
      ; Use other handler, like an anchor handler
      (t
        (defq hndl (gets fmap (gets config :handler)))
        (if hndl
            (sets! config :log_lvl (gets hndl :level))
            (throw "Can't find config handler " config))))
    ; Capture configuration locally
    (sets! registra chsh config)
    (service-send-ser (gets config :reciever) +log_event_registered+ config))

  ; Log Service Processing loop
  (catch (while active
    (debug-write "Log Service Loop")
    (cond
      ; Shutdown (admin)
      ((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +log_event_shutdown+)
        (log-write (gets srvc_fh :handle) " Shutting down ")
        ; (log-write DEBUG " Shutting down ")
        (setq active nil))

      ; Ping aliveness
      ((= id +log_event_ping+)
       (defq
         rcvr (slice +rega_msg_receiver+
                     (const (+ +rega_msg_receiver+ net_id_size)) msg))
       (log-write (gets srvc_fh :handle) " Received ping "))

      ; Information request about registrations (admin)
      ; Returns ack with
      ((= id +log_event_query_anchor_config+)
       (debug-write " Anchor query config" msg)
       (defq
         rcvr (slice +rega_msg_receiver+ (const (+ +rega_msg_receiver+ net_id_size)) msg)
         nm   (slice +rega_msg_data+ -1 msg)
         akw  (kw nm)
         fhit (kvmap-has-prefix? fmap nm))
       (debug-write "   for name " nm " kw " akw " hit? " fhit)
       (service-send
         rcvr
         +log_event_anchor_info+
         (cat (if (gets (gets registry :handlers) akw) "true" "false")
              "," (if fhit fhit "false"))))

      ; Registration (anchor) using persistent configuration
      ; This is called when anchor detects a configuration is
      ; already registered but the handler is not started

      ((= id +log_event_register_anchor_activate+)
       ; Get configuration for anchor
       (debug-write " Anchor Activate" msg)
       (defq
         rcvr  (slice +rega_msg_receiver+
                      (const (+ +rega_msg_receiver+ net_id_size)) msg)
         msgd  (deser-anchor-inbound (slice +rega_msg_data+ -1 msg))
         nmkw  (kw (gets msgd :name))
         hnkw  (gets msgd :handler)
         hndl  (gets (gets registry :handlers) nmkw)
         chndl (copy hndl))
       (log-write (gets srvc_fh :handle) " Activating anchor " nmkw)
        ; Start handler with handler keyname provided
       (sets! fmap hnkw (initialize-logfile-handler chndl))
       (register-logger msgd))

      ; Registration (anchor) using persistent configuration
      ; This is called when the anchor detects a configuration
      ; is already registered and handler is active

      ((= id +log_event_register_anchor+)
       (debug-write " Reusing anchor " msg)
       (log-write (gets srvc_fh :handle) " Register reuse anchor ")
       (defq
         rcvr  (slice +rega_msg_receiver+
                      (const (+ +rega_msg_receiver+ net_id_size)) msg))
       (register-logger
         (deser-anchor-inbound (slice (+ +rega_msg_data+ net_id_size) -1 msg))))

      ; Registration (anchor) sends ack with handler configuration
      ; This is called when the anchor determines that it's
      ; configuration has not been persisted so:
      ; Add to registry and persist
      ; Activate handler and add to fmap

      ((= id +log_event_register_anchor_with_configuration+)
       (debug-write " Register anchor config " msg)
       (defq
         rcvr  (slice +rega_msg_receiver+
                      (const (+ +rega_msg_receiver+ net_id_size)) msg)
         msgd  (deser-anchor-inbound (slice +rega_msg_data+ -1 msg))
         nm   (gets msgd :name)
         nmkw (kw nm)
         hnkw (gets msgd :key_name))
       (drop! msgd :name)
       (drop! msgd :key_name)
       (log-write (gets srvc_fh :handle) " Configuring anchor " nm)
       ; Register handler
       (register-log-handler registry nmkw msgd)
       ; Add to handler execution map
       (sets! fmap hnkw (initialize-logfile-handler msgd))
       ; Register the logger instance
       (register-logger (log-registration nm hnkw rcvr)))

      ((= id +log_event_drop_anchor+)
       (defq anckw (kw (slice mail_msg_data -1 msg)))
       (log-write (gets srvc_fh :handle) " Dropping anchor " anckw))

      ; Registration (client)
      ((= id +log_event_register+)
       (defq msgd (deser-inbound msg))
       (register-logger msgd))

      ; Log Message (client)
      ((= id +log_event_logmsg+)
       (log-msg-writer msg))
      ; Should throw exception
      (t
        (log-write (gets srvc_fh :handle) " Unknown " msg)
        ; (log-write DEBUG " Unknown " msg)
        )))
    (debug-write "Exception " _))
  (mail-forget entry)
)