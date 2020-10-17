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
  (defq entry (mail-declare +logging_srvc_name+ (task-mailbox) "Logging Service 0.1"))

  ; Process configuration file
  (bind '(fs fcfg? conf) (process-log-cfg))

  ; Setup general purpose information
  (defq
    registra  (hmap)
    active    t)

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
    (log-write fs " Registering " (getp config :name))
    (stream-flush fs)
    (log-set-cfg config conf)
    (hmap-insert registra (getp config :token) config)
    (log-write fs " Registered " config)
    (stream-flush fs)
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
        (log-write fs " Shutting down ")
        (setq active nil fs nil))
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
        (log-write " Unknown " msg))))
  (mail-forget entry)
)