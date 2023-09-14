;import symbols
(import "./kernel/lisp.inc")

(ffi kernel-stats "sys/kernel/lisp_stats" 0)
(ffi load-path "sys/load/lisp_path" 0)
(ffi task-netid "sys/task/lisp_mailbox" 0)
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
	; (alloc-select size) -> ((task-netid) [temp_mbox] ...)
	(defq select (list (task-netid)))
	(while (> (setq size (dec size)) 0)
		(push select (mail-alloc-mbox)))
	select)

(defun free-select (select)
	; (free-select select)
	(each! 1 -1 mail-free-mbox (list select)))

(defun to-net-id (id)
	; (to-net-id service_id) -> net_id
	(bind '(e o) (unzip id (lists2)))
	(setq id (apply cat (map (# (char
		(+ (- (code %0) (ascii-code "a"))
		(<< (- (code %1) (ascii-code "a")) 4)))) e o)))
	(cat (slice 16 -1 id) (slice 0 16 id)))

(defun to-service-id (id)
	; (to-service-id net_id) -> service_id
	(defq id (apply cat (map (# (cat
		(char (+ (logand 0x0f (code %0)) (ascii-code "a")))
		(char (+ (logand 0x0f (>> (code %0) 4)) (ascii-code "a"))))) id)))
	(if (= (length id) +net_id_size) (cat (slice 16 -1 id) (slice 0 16 id)) id))

(defun open-task (task node mode key_num reply)
	; (open-task task node mode key_num reply)
	(mail-send (cat (char 0 +long_size) node)
		(cat (char mode +long_size) (char key_num +long_size)
			reply task (char 0))))

(defun open-child (task mode)
	; (open-child task mode) -> net_id
	(defq mbox (mail-alloc-mbox))
	(open-task task (slice +long_size -1 (task-netid)) mode 0 mbox)
	(defq id (getf (mail-read mbox) +kn_msg_reply_id))
	(mail-free-mbox mbox) id)

(defun open-remote (task node mode)
	; (open-remote task node mode) -> net_id
	(defq mbox (mail-alloc-mbox))
	(open-task task node mode 0 mbox)
	(defq id (getf (mail-read mbox) +kn_msg_reply_id))
	(mail-free-mbox mbox) id)

(defun open-pipe (tasks)
	; (open-pipe tasks) -> ([net_id | 0] ...)
	(defq ids (cap (length tasks) (list)) mbox (mail-alloc-mbox)
		index -1 tail (task-netid))
	(while (< (setq index (inc index)) (length tasks))
		(open-task (elem-get index tasks) (slice +long_size -1 tail) +kn_call_child 0 mbox)
		(defq tail (getf (mail-read mbox) +kn_msg_reply_id))
		(push ids tail)
		(if (= 0 (get-long tail 0)) (setq tail (task-netid))))
	(mail-free-mbox mbox) ids)

(defun task-nodeid ()
	(slice +mailbox_id_size -1 (task-netid)))

;jit helper, lock file is not very effective here...
(defun jit (prefix file products)
	(bind '(_ *cpu* *abi*) (map sym (split (load-path) "/")))
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
