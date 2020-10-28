;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logservice.inc")
(import "lib/logging/logcommons.inc")
(import "lib/yaml-data/yaml-data.lisp")
(import "apps/logger/logutils.lisp")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  ; Setup general purpose information
  (defq
    registra  (hmap)
    active    t
    entry (mail-declare +logging_srvc_name+ (task-mailbox) "Logging Service 0.3")
    ; DEBUG (file-stream "./logs/DEBUG_SERVICE.log" file_open_append)
    )

  ; (defun-bind debug-write (&rest _)
  ;   (write DEBUG (apply str (push _ +nl+)))
  ;   (stream-flush DEBUG))
  (defun-bind debug-write (&rest _))

  ; Process configuration files
  (bind '(srvc_fh fcfg? conf fmap registry) (process-log-cfg))

  (log-write (gets srvc_fh :handle) " Starting LOG_SERVICE")

  (defun-bind log-handle (cfg)
    (gets cfg :handle))

  (defun-bind logfs (fsmap config)
    (log-handle (hmap-find fsmap (gets config :handler))))

  (defun-bind log-msg-writer (msg)
    ; (log-msg-writer mail-message) -> stream
    (defq
      msgd (deser-inbound msg)
      cnfg (hmap-find registra (gets msgd :module))
      sstrm (logfs fmap cnfg))
    (log-write sstrm (str
                 " ["(log-level-string cnfg (gets msgd :msg-level))"] "
                 (gets cnfg :name)": ") (gets msgd :message)))

  (defun-bind service-send (toclient command strng)
    ; (service-send mailbox command data)
    ; Sends a mail message from log_service to
    ; mailbox
    (mail-send (cat (char command long_size) strng) toclient))

  (defun-bind service-send-ser (toclient command data)
    ; (service-send-ser mailbox command data)
    ; Serializes data and calls service-send
    (service-send toclient command (str (yaml-xser data))))

  (defun-bind register-logger (config)
    ; (register-logger properties) -> ?
    (log-write (gets srvc_fh :handle) " Registering " (gets config :name))
    (stream-flush (gets srvc_fh :handle))
    ; Basics
    (sets-pairs! config
      :log_lvl :info
      :token (hash config)
      :levels (getp-in conf :logging :levels))
    ; Resolve handler
    (case (gets config :handler)
      ; Keyword cases
      ((:console)
       (defq hndl (getp-in conf :logging :loggers :console :handler))
       (sets-pairs! config
          :log_lvl (getp-in conf :logging :handlers hndl :level)
          :handler hndl))
      ((:file)
       (defq hndl (getp-in conf :logging :loggers :file :handler))
       (sets-pairs! config
          :log_lvl (gets (hmap-find fmap hndl) :level)
          :handler hndl))
      ((:system)
       (defq hndl (getp-in conf :logging :loggers :system :handler))
       (sets-pairs! config
          :log_lvl (gets (hmap-find fmap hndl):level)
          :handler hndl))
      ; Use other handler, like an anchor handler
      (t
        (defq hndl (gets fmap (gets config :handler)))
        (if hndl
            (sets! config :log_lvl (gets hndl :level))
            (throw "Can't find config handler " config))))
    ; Capture configuration locally
    (hmap-insert registra (gets config :token) config)
    (service-send-ser (gets config :reciever) +log_event_registered+ config))

  ; Log Service Processing loop
  (while active
    (cond
      ; Shutdown (admin)
      ((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +log_event_shutdown+)
        (log-write (gets srvc_fh :handle) " Shutting down ")
        ; (log-write DEBUG " Shutting down ")
        (setq active nil))
      ; Information request about registrations (admin)
      ; Returns ack with
      ((= id +log_event_query_anchor_config+)
       (defq
         rcvr (get-long msg +rega_msg_receiver+)
         akw  (kw (slice +rega_msg_data+ -1 msg)))
       (service-send
         rcvr
         +log_event_anchor_info+
         (cat (if (gets (gets registry :handlers) akw) "true" "false")
              "," (if (gets fmap akw) "true" "false"))))

      ; Registration (anchor) using persistent configuration
      ; This is called when the anchor detects a configuration
      ; is already registered
      ; TODO: Checks if there is no open fmap and the configuration of
      ; the anchor is not a console configuration. Opens/adds
      ; file_handler to fmap if not open and not console handler
      ; TODO: Responds with standard logger configuration
      ((= id +log_event_register_anchor+)
       (defq rcvr (get-long msg +rega_msg_receiver+))
       (service-send
         rcvr
         +log_event_registered+
         msg))

      ; Registration (anchor) sends ack with handler configuration
      ; This is called when the anchor determines that it's
      ; configuration has not been persisted so:
      ; Add to registry and persists
      ; TODO: Add a file-handler to fmap using ? as key if not :console
      ; TODO: Responds with standard logger configuration
      ((= id +log_event_register_anchor_with_configuration+)
       (defq
         rcvr (get-long msg +rega_msg_receiver+)
         msgd (deser-anchor-inbound msg)
         nm   (gets msgd :name)
         nmkw (kw nm)
         hnkw (gets msgd :key_name)
         cfgc (copy msgd))
       (pdrop! cfgc :name)
       (pdrop! cfgc :key_name)
       ; Register handler
       (register-log-handler registry nmkw msgd)
       ; Add to handler execution map
       (sets! fmap hnkw (initialize-logfile-handler cfgc))
       ; Register the logger instance
       (register-logger (log-registration nm hnkw rcvr)))

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
  (mail-forget entry)
)