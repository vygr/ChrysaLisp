;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ipctest -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/ipc/ipcdefs.inc")

(when (= (length (mail-enquire "IPC")) 0)
  (defq
    active  t
    entry   (mail-declare (task-mailbox) "IPC" "IPC TEST"))
  (catch (while active
    (defq
      id  nil
      msg (mail-read (task-mailbox)))
    (if (list? msg)
      (setq id (first msg) msg (rest msg))
      (setq id (get-long msg ev_msg_target_id)))
    (cond
      ((= id ipc_event_shutdown)
       (setq active nil))
      ((= id ipc_event_ping)
       (print msg))))
    (progn
      (setq active nil)))
  (mail-forget entry))
