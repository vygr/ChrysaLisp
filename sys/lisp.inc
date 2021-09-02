;import symbols
(import "sys/kernel/lisp.inc")

(ffi kernel-stats "sys/kernel/lisp_stats" 0)
(ffi load-path "sys/load/lisp_path" 0)
(ffi task-mailbox "sys/task/lisp_mailbox" 0)
(ffi task-sleep "sys/task/lisp_sleep" 0)
(ffi task-slice "sys/task/lisp_timeslice" 0)
(ffi mail-alloc-mbox "sys/mail/lisp_alloc_mbox" 0)
(ffi mail-declare "sys/mail/lisp_declare" 0)
(ffi mail-nodes "sys/mail/lisp_devices" 0)
(ffi mail-enquire "sys/mail/lisp_enquire" 0)
(ffi mail-forget "sys/mail/lisp_forget" 0)
(ffi mail-free-mbox "sys/mail/lisp_free_mbox" 0)
(ffi mail-poll "sys/mail/lisp_poll" 0)
(ffi mail-validate "sys/mail/lisp_validate" 0)
(ffi mail-read "sys/mail/lisp_read" 0)
(ffi mail-select "sys/mail/lisp_select" 0)
(ffi mail-send "sys/mail/lisp_send" 0)
(ffi mail-timeout "sys/mail/lisp_timeout" 0)

(structure +mail_timeout 0
	(long time id))

(defun alloc-select (size)
	; (alloc-select size) -> (list (task-mailbox) [temp_mbox] ...)
	(defq select (list (task-mailbox)))
	(while (> (setq size (dec size)) 0)
		(push select (mail-alloc-mbox)))
	select)

(defun free-select (select)
	; (free-select select)
	(each! 1 -1 mail-free-mbox (list select)))

(defun to-net-id (sid)
	; (to-net-id service_id) -> net_id
	(bind '(e o) (unzip sid (list (list) (list))))
	(apply cat (map (# (char
		(+ (- (code %0) (ascii-code "a"))
		(<< (- (code %1) (ascii-code "a")) 4)))) e o)))

(defun to-service-id (nid)
	; (to-service-id net_id) -> service_id
	(apply cat (map (# (cat
		(char (+ (logand 0x0f (code %0)) (ascii-code "a")))
		(char (+ (logand 0x0f (>> (code %0) 4)) (ascii-code "a"))))) nid)))

(defun open-task (task node mode key_num reply)
	; (open-task task node mode key_num reply)
	(mail-send (cat (char 0 +long_size) node)
		(cat (char key_num +long_size) reply
			(char mode +long_size) task (char 0))))

(defun open-child (task mode)
	; (open-child task mode) -> str
	(defq mbox (mail-alloc-mbox))
	(open-task task (slice +long_size -1 (task-mailbox)) mode 0 mbox)
	(defq id (getf (mail-read mbox) +kn_msg_reply_id))
	(mail-free-mbox mbox) id)

(defun open-remote (task node mode)
	; (open-remote task node mode) -> str
	(defq mbox (mail-alloc-mbox))
	(open-task task node mode 0 mbox)
	(defq id (getf (mail-read mbox) +kn_msg_reply_id))
	(mail-free-mbox mbox) id)

(defun open-pipe (tasks)
	; (open-pipe tasks) -> (str ...)
	(defq devices (mail-nodes) ids (cap (length tasks) (list))
		mbox (mail-alloc-mbox) index -1 tail (task-mailbox))
	(while (< (setq index (inc index)) (length tasks))
		(open-task (elem index tasks) (slice +long_size -1 tail) +kn_call_child 0 mbox)
		(defq tail (getf (mail-read mbox) +kn_msg_reply_id))
		(push ids tail))
	(mail-free-mbox mbox) ids)

;jit helper, lock file is not very effective here...
(defun jit (prefix file products)
	(bind '(_ *cpu* *abi*) (map sym (split (load-path) "/")))
	(defq obj_prefix (cat "obj/" *cpu* "/" *abi* "/" prefix)
		lock_file (cat obj_prefix "lock") file (cat prefix file))
	(while (/= (age lock_file) 0) (task-sleep 1000))
	(when (some (# (> (age file) (age (cat obj_prefix %0)))) products)
		(save "lock" lock_file)
		(within-compile-env (lambda () (include file)))
		(pii-remove lock_file)))

(enums +file_open 0
	(enum read write append))
