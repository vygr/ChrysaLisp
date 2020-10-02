;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/logging/logging.inc")

(defq buf_keys (list) buf_list (list) buf_index nil id t)

;single instance only
(when (= (length (mail-enquire +logging_srvc_name+)) 0)
  (mail-declare +logging_srvc_name+ (task-mailbox))

 (structure 'log_msg 0
  (long 'command)
  (offset 'data))

(defq fs (file-stream "logmsg.log" file_open_write))

(while id
  (cond
    ;close ?
    ((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +log_event_shutdown+)
      (write fs (str "Shutting down" +nl+ +eof+))
      (stream-flush fs)
      (setq id nil fs nil))
    ; Registration
    ((= id +log_event_register+)
     (defq
       ; cmd (get-long msg log_msg_command)
       msg  (slice log_msg_data -1 msg)
       msgd (yaml-xdeser (string-stream msg)))
     (write fs (str "Register " msg +nl+)))
    ; New logmsg
    ((= id +log_event_logmsg+)
      (write fs (str "Logmsg " msg +nl+))

      ; (defq reply_id (get-long msg log_msg_reply_id)
      ;   tcb (get-long msg log_msg_tcb)
      ;   data (slice log_msg_data -1 msg)
      ;   key (sym (str (>> reply_id 32) ":" tcb))
      ;   index (find-rev key buf_keys))
   ;    (print msg)
      )
    ;otherwise
    (t
      (write fs (str "Unknown " msg +nl+)))))
  (mail-forget +logging_srvc_name+ (task-mailbox))
)