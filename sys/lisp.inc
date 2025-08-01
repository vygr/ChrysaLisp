;import symbols
(import "./kernel/lisp.inc")

(ffi "sys/kernel/lisp_stats" kernel-stats)
(ffi "sys/load/lisp_path" load-path)
(ffi "sys/task/lisp_mailbox" task-mbox)
(ffi "sys/mail/lisp_netid" mail-mbox)
(ffi "sys/task/lisp_count" task-count)
(ffi "sys/task/lisp_sleep" task-sleep)
(ffi "sys/task/lisp_timeslice" task-slice)
(ffi "sys/mail/lisp_declare" mail-declare)
(ffi "sys/mail/lisp_devices" mail-nodes)
(ffi "sys/mail/lisp_enquire" mail-enquire)
(ffi "sys/mail/lisp_forget" mail-forget)
(ffi "sys/mail/lisp_poll" mail-poll)
(ffi "sys/mail/lisp_validate" mail-validate)
(ffi "sys/mail/lisp_read" mail-read)
(ffi "sys/mail/lisp_select" mail-select)
(ffi "sys/mail/lisp_send" mail-send)
(ffi "sys/mail/lisp_timeout" mail-timeout)

(structure +mail_timeout 0
	(long time id))

(defun task-mboxes (size)
	; (task-mboxes size) -> ((task-mbox) [temp_mbox] ...)
	(defq select (list (task-mbox)))
	(while (> (-- size) 0) (push select (mail-mbox)))
	select)

(defun id-decode (id)
	; (id-decode id) -> id
	(cond
		((eql id "") "")
		(:t (bind '(e o) (unzip id 2))
			(apply (const cat) (map (# (char
				(+ (- (code %0) (ascii-code "a"))
				(<< (- (code %1) (ascii-code "a")) 4)))) e o)))))

(defun id-encode (id)
	; (id-encode id) -> id
	(cond
		((eql id "") "")
		(:t (apply (const cat) (map (# (cat
			(char (+ (logand 0x0f (code %0)) (ascii-code "a")))
			(char (+ (logand 0x0f (>> (code %0) 4)) (ascii-code "a"))))) id)))))

(defun to-net-id (id)
	; (to-net-id service_id) -> net_id
	(setq id (id-decode id))
	(cat (slice id 16 -1) (slice id 0 16)))

(defun to-service-id (id)
	; (to-service-id net_id) -> service_id
	(setq id (id-encode id))
	(if (= (length id) +net_id_size) (cat (slice id 16 -1) (slice id 0 16)) id))

(defun open-task (task node mode key_num reply)
	; (open-task task node mode key_num reply)
	(mail-send (cat (char 0 +long_size) node)
		(cat (char mode +long_size) (char key_num +long_size)
			reply task (char 0))))

(defun open-child (task mode)
	; (open-child task mode) -> net_id
	(defq mbox (mail-mbox))
	(open-task task (slice (task-mbox) +long_size -1) mode 0 mbox)
	(getf (mail-read mbox) +kn_msg_reply_id))

(defun open-remote (task node mode)
	; (open-remote task node mode) -> net_id
	(defq mbox (mail-mbox))
	(open-task task node mode 0 mbox)
	(getf (mail-read mbox) +kn_msg_reply_id))

(defun open-pipe (tasks)
	; (open-pipe tasks) -> ([net_id | 0] ...)
	(defq ids (cap (length tasks) (list)) mbox (mail-mbox)
		index -1 tail (task-mbox))
	(while (< (++ index) (length tasks))
		(open-task (elem-get tasks index) (slice tail +long_size -1) +kn_call_child 0 mbox)
		(defq tail (getf (mail-read mbox) +kn_msg_reply_id))
		(push ids tail)
		(if (= 0 (get-long tail 0)) (setq tail (task-mbox))))
	ids)

(defun task-nodeid ()
	(slice (task-mbox) +mailbox_id_size -1))

(defun task-timeout (time)
	; (task-timeout s) -> ns
	(if (starts-with "obj/vp64" (load-path)) (* time 10000000) (* time 1000000)))

;jit helper, lock file is not very effective here...
(defun jit (prefix file products)
	(bind '(_ *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(defq obj_prefix (cat "obj/" *cpu* "/" *abi* "/" prefix)
		lock_file (cat obj_prefix "lock") file (cat prefix file))
	(while (/= (age lock_file) 0) (task-sleep 1000))
	(when (some (# (> (age file) (age (cat obj_prefix %0)))) products)
		(catch
			(progn
				(save "lock" lock_file)
				(within-compile-env (lambda () (include file)))
				(pii-remove lock_file))
			(progn (pii-remove lock_file) :nil))))

(enums +file_open 0
	(enum read write append))
