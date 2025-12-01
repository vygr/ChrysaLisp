;import symbols
(import "./kernel/lisp.inc")
(import "service/lock/app.inc")

(structure +mail_timeout 0
	(long time id))

(defun task-mboxes (size)
	; (task-mboxes size) -> ((task-mbox) [temp_mbox] ...)
	(defq select (list (task-mbox)))
	(while (> (-- size) 0) (push select (mail-mbox)))
	select)

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

;jit helper
(defun jit (prefix file products)
	(bind '(_ *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(defq obj_prefix (cat "obj/" *cpu* "/" *abi* "/" prefix)
		file (cat prefix file) file_age (age file))
	(when (some (# (> file_age (age (cat obj_prefix %0)))) products)
		(lock-claim-rpc obj_prefix)
		(when (some (# (> file_age (age (cat obj_prefix %0)))) products)
			(catch (within-compile-env (# (include file))) :nil))
		(lock-release-rpc obj_prefix)))

(enums +file_open 0
	(enum read write append))
