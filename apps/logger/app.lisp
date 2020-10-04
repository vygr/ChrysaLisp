;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logger - ChrysaLisp Logging Service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logging.inc")
(import "lib/hmap/hmap.inc")
(import "lib/date/date.inc")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  (mail-declare +logging_srvc_name+ (task-mailbox))

  ; Setup timezone for now
  (timezone-init "America/New_York")

  (defq
    fs  (file-stream "logmsg.log" file_open_write)
    reg (hmap)
    active t)

  (defun log-write (&rest _)
    ; (log-write ....) -> stream
    ; Wrap timestamp and nl to '_' arguments
    (setq _ (insert (push _ +nl+) 0 (list (str "[" (encode-date (date)) "] "))))
    (write fs (apply str _))
    (stream-flush fs))

  (defun-bind deser-inbound (msg)
    (yaml-xdeser (write (string-stream (cat "")) (slice mail_msg_data -1 msg))))

  ; (log-write "apps/logger/logsrvc.yaml " (age "apps/logger/logsrvc.yaml"))

  (defun-bind register-logger (config)
    ; (register-logger properties) -> ?
    (defq hsh (hash config))
    (log-write "Registering" (getp config :name))
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
        (log-write "Shutting down")
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
        (log-write "Unknown " msg))))
  (mail-forget +logging_srvc_name+ (task-mailbox))
)