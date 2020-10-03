;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logging.inc")

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  (mail-declare +logging_srvc_name+ (task-mailbox))

  (structure 'log_msg 0
    (long 'command)
    (offset 'data))

  (defq
    fs  (file-stream "logmsg.log" file_open_write)
    reg (properties)
    active t)

  (write fs (str "logsrvc.yaml " (age "logsrvc.yaml") +nl+))
  (write fs (str "logger/logsrvc.yaml " (age "logger/logsrvc.yaml") +nl+))
  (write fs (str "apps/logger/logsrvc.yaml " (age "apps/logger/logsrvc.yaml") +nl+))

  (while active
    (cond
      ;close ?
      ((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +log_event_shutdown+)
        (write fs (str "Shutting down" +nl+ +eof+))
        (stream-flush fs)
        (setq active nil fs nil))
      ; Registration
      ((= id +log_event_register+)
       (defq
         msgs  (slice log_msg_data -1 msg)
         msgd (yaml-xdeser (write (string-stream (cat "")) msgs)))
       (write fs (str "Registering " +nl+))
       (write fs (str "reg-name " (getp msgd :name) +nl+))
       (write fs (str "reg-recv " (getp msgd :reciever) +nl+))
       (write fs (str "reg-hash " (hash msgd) +nl+))
       )
      ; New logmsg
      ((= id +log_event_logmsg+)
        (write fs (str "Logmsg " msg +nl+)))
      ;otherwise
      (t
        (write fs (str "Unknown " msg +nl+)))))
  (mail-forget +logging_srvc_name+ (task-mailbox))
)