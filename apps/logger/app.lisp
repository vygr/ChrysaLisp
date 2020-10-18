;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logging.inc")
(import "lib/hmap/hmap.inc")
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

  ; Setup general purpose information
  (defq
    registra  (hmap)
    active    t)

  (log-write (getp srvc_fh :handle) " Starting LOG_SERVICE")
  (debug-write "Starting LOG_SERVICE")

  (defun-bind log-msg-writer (sstrm msg)
    ; (log-msg-writer stream mail-message) -> stream
    (defq
      msgd (deser-inbound msg)
      cnfg (hmap-find registra (getp msgd :module)))
    (log-write sstrm (str
                 " ["(log-level-string cnfg (getp msgd :msg-level))"] "
                 (getp cnfg :name)": ") (getp msgd :message)))

  (defun-bind register-logger (config)
    ; (register-logger properties) -> ?
    (log-write (getp srvc_fh :handle) " Registering " (getp config :name))
    (stream-flush (getp srvc_fh :handle))
    (log-set-cfg config conf fmap)
    (hmap-insert registra (getp config :token) config)
    (log-write (getp srvc_fh :handle) " Registered " config)
    (stream-flush (getp srvc_fh :handle))
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
       (log-msg-writer fs msg))
      ; Should throw exception
      (t
        (log-write (getp srvc_fh :handle) " Unknown " msg)
        ; (log-write DEBUG " Unknown " msg)
        )))
  (mail-forget entry)
)