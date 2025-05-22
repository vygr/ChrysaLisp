;import symbols
(import "./kernel/lisp.inc")

(ffi "sys/kernel/lisp_stats" kernel-stats)
(ffi "sys/load/lisp_path" load-path)
(ffi "sys/task/lisp_mailbox" task-netid)
(ffi "sys/task/lisp_count" task-count)
(ffi "sys/task/lisp_sleep" task-sleep)
(ffi "sys/task/lisp_timeslice" task-slice)
(ffi "sys/mail/lisp_alloc_mbox" mail-alloc-mbox)
(ffi "sys/mail/lisp_declare" mail-declare)
(ffi "sys/mail/lisp_devices" mail-nodes)
(ffi "sys/mail/lisp_enquire" mail-enquire)
(ffi "sys/mail/lisp_forget" mail-forget)
(ffi "sys/mail/lisp_free_mbox" mail-free-mbox)
(ffi "sys/mail/lisp_poll" mail-poll)
(ffi "sys/mail/lisp_validate" mail-validate)
(ffi "sys/mail/lisp_read" mail-read)
(ffi "sys/mail/lisp_select" mail-select)
(ffi "sys/mail/lisp_send" mail-send)
(ffi "sys/mail/lisp_timeout" mail-timeout)

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
	(each! (const mail-free-mbox) (list select) 1))

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
	(defq mbox (mail-alloc-mbox))
	(open-task task (slice (task-netid) +long_size -1) mode 0 mbox)
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
		(open-task (elem-get tasks index) (slice tail +long_size -1) +kn_call_child 0 mbox)
		(defq tail (getf (mail-read mbox) +kn_msg_reply_id))
		(push ids tail)
		(if (= 0 (get-long tail 0)) (setq tail (task-netid))))
	(mail-free-mbox mbox) ids)

(defun task-nodeid ()
	(slice (task-netid) +mailbox_id_size -1))

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
