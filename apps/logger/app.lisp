;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logging.inc")
(import "lib/hmap/hmap.inc")
(import "lib/date/date.inc")
(import "lib/yaml-data/yaml-data.lisp")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  (mail-declare +logging_srvc_name+ (task-mailbox))

  ; Setup timezone for now
  (timezone-init "America/New_York")

  ; Setup general purpose information
  (defq
<<<<<<< HEAD
    fs  (file-stream "./logs/logservice.log" file_open_write_append)
=======
    fs  (file-stream "logmsg.log" file_open_write)
>>>>>>> parent of b4694fe0... log-msg writer
    reg (hmap)
    lup (hmap)
    active t)

<<<<<<< HEAD
  ; Populate lookups
  (hmap-insert lup +log_message_debug+ "DEBUG")
  (hmap-insert lup +log_message_info+ "INFO")
  (hmap-insert lup +log_message_warning+ "WARN")
  (hmap-insert lup +log_message_error+ "ERROR")
  (hmap-insert lup +log_message_critical+ "CRITICAL")

  (defun-bind log-write (&rest _)
=======
  (defun log-write (&rest _)
>>>>>>> parent of b4694fe0... log-msg writer
    ; (log-write ....) -> stream
    ; Wrap timestamp and nl to '_' arguments
    (setq _ (insert (push _ +nl+) 0 (list (encode-date (date)))))
    (write fs (apply str _))
    (stream-flush fs))

<<<<<<< HEAD
  (defun-bind log-msg-writer (msg)
    ; (log-msg-writer mail-message) -> stream
    (defq
      msgd (deser-inbound msg)
      cnfg (hmap-find reg (getp msgd :module)))
    (log-write (str
                 " [" (hmap-find lup (getp msgd :msg-type))"] "
                 (getp cnfg :name)": ") (getp msgd :message)))
=======
  (defun-bind deser-inbound (msg)
    (yaml-xdeser (write (string-stream (cat "")) (slice mail_msg_data -1 msg))))

  ; (log-write "apps/logger/logsrvc.yaml " (age "apps/logger/logsrvc.yaml"))
>>>>>>> parent of b4694fe0... log-msg writer

  (defun-bind register-logger (config)
    ; (register-logger properties) -> ?
    (defq hsh (hash config))
<<<<<<< HEAD
    (log-write " Registering " (getp config :name))
=======
    (log-write "Registering" (getp config :name))
>>>>>>> parent of b4694fe0... log-msg writer
    (hmap-insert reg hsh config)
    (setp! config :token hsh t)
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
        (log-write " Shutting down ")
        (setq active nil fs nil))
      ; Information request about registrations (admin)
      ; Registration (client)
      ((= id +log_event_register+)
       (defq msgd (deser-inbound msg))
       (register-logger msgd))
      ; Reconfiguration (client)
      ; Log Message (client)
      ((= id +log_event_logmsg+)
        (defq
          msgd  (deser-inbound msg)
          mname (getp (hmap-find reg (getp msgd :module)) :name))
        (log-write mname (getp msgd :message) (slice mail_msg_data -1 msg)))
      ; Should throw exception
      (t
        (log-write " Unknown " msg))))
  (mail-forget +logging_srvc_name+ (task-mailbox))
)