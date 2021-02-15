;imports
(import "sys/lisp.inc")

(structure '+select 0
	(byte 'main+ 'timeout+))

(defun main ()
	(defq select (list (task-mailbox) (mail-alloc-mbox)) id t +timeout+ 5000000)
	(while id
		(mail-timeout (elem +select_timeout+ select) +timeout+)
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((or (= idx +select_timeout+) (eql msg ""))
				;timeout or quit
				(setq id nil))
			((= idx +select_main+)
				;main mailbox, reset timeout and reply with result
				(mail-timeout (elem +select_timeout+ select) 0)
				(bind '(task_count mem_used) (kernel-stats))
				(mail-send msg (cat
					(slice +long_size+ -1 (task-mailbox))
					(char task_count +int_size+)
					(char mem_used +int_size+))))))
	(mail-free-mbox (pop select)))
