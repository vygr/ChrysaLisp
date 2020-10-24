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
  (defq
    entry (mail-declare +logging_srvc_name+ (task-mailbox) "Logging Service 0.3")
    ; DEBUG (file-stream "./logs/DEBUG_SERVICE.log" file_open_append)
    )

  ; (defun-bind debug-write (&rest _)
  ;   (write DEBUG (apply str (push _ +nl+)))
  ;   (stream-flush DEBUG))
  (defun-bind debug-write (&rest _))

  ; Process configuration file
  (bind '(srvc_fh fcfg? conf fmap) (process-log-cfg))
  (log-write (gets srvc_fh :handle) " Starting LOG_SERVICE")

  ; Setup general purpose information
  (defq
    registra  (hmap)
    active    t)

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
      ((:inherit))
      (t))
    ; Capture configuration locally
    (hmap-insert registra (gets config :token) config)
    (mail-send
      (cat
        (char +log_event_registered+ long_size)
        (str (yaml-xser config)))
      (gets config :reciever)))

  ; Log Service Processing loop
  (while active
    (cond
      ; Shutdown (admin)
      ((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +log_event_shutdown+)
        (log-write (gets srvc_fh :handle) " Shutting down ")
        ; (log-write DEBUG " Shutting down ")
        (setq active nil))
      ; Registration (anchor)
      ((= id +log_event_register_anchor+)
       (log-write (gets srvc_fh :handle) " Registering " msg))
      ; Information request about registrations (admin)
      ((= id +log_event_query_config+)
       (log-write (gets srvc_fh :handle) " Querying " msg))
      ; Registration (client)
      ((= id +log_event_register+)
       (defq msgd (deser-inbound msg))
       (register-logger msgd))
      ; Reconfiguration (client)
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