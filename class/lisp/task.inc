;;;;;;;;;;
; Per Task
;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Kernel & System
;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "sys/kernel/lisp_stats" kernel-stats)
; (kernel-stats) -> (task_count mem_used mem_avail max_stack)
(ffi "sys/load/lisp_path" load-path)
; (load-path) -> path

;;;;;;;;;;;;;;;;;;;;
; Task Management
;;;;;;;;;;;;;;;;;;;;
(ffi "sys/task/lisp_mailbox" task-mbox)
; (task-mbox) -> netid
(ffi "sys/task/lisp_count" task-count)
; (task-count bias) -> count
(ffi "sys/task/lisp_sleep" task-sleep)
; (task-sleep usec)
(ffi "sys/task/lisp_timeslice" task-slice)
; (task-slice)

;;;;;;;;;;;;;;;;;;;;;;
; Mail & Messaging
;;;;;;;;;;;;;;;;;;;;;;
(ffi "sys/mail/lisp_netid" mail-mbox)
; (mail-mbox) -> netid
(ffi "sys/mail/lisp_declare" mail-declare)
; (mail-declare mbox name info) -> key
(ffi "sys/mail/lisp_devices" mail-nodes)
; (mail-nodes) -> nodeids
(ffi "sys/mail/lisp_enquire" mail-enquire)
; (mail-enquire prefix) -> netids
(ffi "sys/mail/lisp_forget" mail-forget)
; (mail-forget key)
(ffi "sys/mail/lisp_poll" mail-poll)
; (mail-poll mboxs) -> :nil | idx
(ffi "sys/mail/lisp_validate" mail-validate)
; (mail-validate mbox) -> :t | :nil
(ffi "sys/mail/lisp_read" mail-read)
; (mail-read mbox) -> :nil | msg
(ffi "sys/mail/lisp_select" mail-select)
; (mail-select mboxs) -> idx
(ffi "sys/mail/lisp_send" mail-send)
; (mail-send mbox obj)
(ffi "sys/mail/lisp_timeout" mail-timeout)
; (mail-timeout mbox ns id) -> mbox

(defq *debug_state* :nil *stack_frame* :nil *profile_map* :nil)

(import "sys/lisp.inc")
(import "class/lisp.inc")
