(import "sys/lisp.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun main ()
	(defq select (alloc-select +select_size) running t +timeout 5000000)
	(while running
		(mail-timeout (elem-get +select_timeout select) +timeout 0)
		(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with info
				(mail-timeout (elem-get +select_timeout select) 0 0)
				(bind '(task_count mem_used mem_avail) (kernel-stats))
				(mail-send msg (setf-> (str-alloc +reply_size)
					(+reply_node (slice +long_size -1 (task-mailbox)))
					(+reply_task_count task_count)
					(+reply_mem_alloc (- mem_used mem_avail))
					(+reply_mem_used mem_used))))))
	(free-select select))
