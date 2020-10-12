;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logging.inc")
(import "lib/hmap/hmap.inc")
(import "lib/yaml-data/yaml-data.lisp")

(defq
  +logs_path+ "./logs/"
  +log_suffix+ ".log")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  (mail-declare +logging_srvc_name+ (task-mailbox))

  (defun-bind logfile_setup (fhandler)
    )

  ; Setup general purpose information
  (defq
    fs    (file-stream "./logs/logservice.log" file_open_append)
    reg   (hmap)
    conf  (first (yaml-read "./apps/logger/logsrvc.yaml"))
    lup   (getp-in conf :logging :levels)
    hand  (getp-in conf :logging :handlers)
    logrs (getp-in conf :logging :loggers :default :handler)
    active t)

  (defun-bind log-msg-writer (sstrm msg)
    ; (log-msg-writer stream mail-message) -> stream
    (defq
      msgd (deser-inbound msg)
      cnfg (hmap-find reg (getp msgd :module)))
    (log-write sstrm (str
                 " [" (getp msgd :msg-level)"] "
                 (getp cnfg :name)": ") (getp msgd :message)))

  (defun-bind register-logger (config)
    ; (register-logger properties) -> ?
    (defq hsh (hash config))
    (log-write fs " Registering " (getp config :name))
    (hmap-insert reg hsh config)
    (setp! config :token hsh t)
    ; Use default configuration if not specified
    (setp! config :logger logrs t)
    (setp! config :levels lup t)
    (setp! config :configuration (getp hand (getp config :logger)) t)
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
        (log-write fs " Unknown " msg))))
  (mail-forget +logging_srvc_name+ (task-mailbox))
)