;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logcommons.inc")
(import "lib/yaml-data/yaml-data.lisp")
(import "apps/logger/logutils.lisp")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  (defq
    entry (mail-declare +logging_srvc_name+ (task-mailbox) "Logging Service 0.2")
    ; DEBUG (file-stream "./logs/DEBUG_SERVICE.log" file_open_append)
    )

  ; (defun-bind debug-write (&rest _)
  ;   (write DEBUG (apply str (push _ +nl+)))
  ;   (stream-flush DEBUG))
  (defun-bind debug-write (&rest _))

  ; Process configuration file
  (bind '(srvc_fh fcfg? conf fmap) (process-log-cfg))
  (log-write (getp srvc_fh :handle) " Starting LOG_SERVICE")

  ; Setup general purpose information
  (defq
    registra  (hmap)
    active    t)

  (defun-bind log-handle (cfg)
    (getp cfg :handle))

  (defun-bind logfs (fsmap config)
    (log-handle (hmap-find fsmap (getp config :handler))))

  (defun-bind log-msg-writer (msg)
    ; (log-msg-writer mail-message) -> stream
    (defq
      msgd (deser-inbound msg)
      cnfg (hmap-find registra (getp msgd :module))
      sstrm (logfs fmap cnfg))
    (log-write sstrm (str
                 " ["(log-level-string cnfg (getp msgd :msg-level))"] "
                 (getp cnfg :name)": ") (getp msgd :message)))

  (defun-bind register-logger (config)
    ; (register-logger properties) -> ?
    (log-write (getp srvc_fh :handle) " Registering " (getp config :name))
    (stream-flush (getp srvc_fh :handle))
    ; Basics
    (setsp! config
      :log_lvl :info
      :token (hash config)
      :levels (getp-in conf :logging :levels))
    ; Resolve handler
    (case (getp config :handler)
      ; Keyword cases
      ((:console)
       (defq hndl (getp-in conf :logging :loggers :console :handler))
       (setsp! config
          :log_lvl (getp-in conf :logging :handlers hndl :level)
          :handler hndl))
      ((:file)
       (defq hndl (getp-in conf :logging :loggers :file :handler))
       (setsp! config
          :log_lvl (getp (hmap-find fmap hndl) :level)
          :handler hndl))
      ((:system)
       (defq hndl (getp-in conf :logging :contexts :system :handler))
       (setsp! config
          :log_lvl (getp (hmap-find fmap hndl):level)
          :handler hndl))
      ((:inherit))
      (t))
    ; Capture configuration locally
    (hmap-insert registra (getp config :token) config)
    (mail-send
      (cat
        (char +log_event_registered+ long_size)
        (str (yaml-xser config)))
      (getp config :reciever)))

  ; Log Service Processing loop
  (while active
    (cond
      ; Shutdown (admin)
      ((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +log_event_shutdown+)
        (log-write (getp srvc_fh :handle) " Shutting down ")
        ; (log-write DEBUG " Shutting down ")
        (setq active nil))
      ; Information request about registrations (admin)
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
        (log-write (getp srvc_fh :handle) " Unknown " msg)
        ; (log-write DEBUG " Unknown " msg)
        )))
  (mail-forget entry)
)