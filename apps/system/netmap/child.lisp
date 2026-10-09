(import "lib/net/links.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun main ()
	(defq select (task-mboxes +select_size) running :t +timeout 5000000)
	(while running
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running :nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with this node, its
				;load, and its links
				(mail-timeout (elem-get select +select_timeout) 0 0)
				;a node whose kernel is older than the count of idle time
				;says no time, and is not shown as at work
				(defq links (net-links) stats (kernel-stats) timed (> (length stats) 4))
				(mail-send msg (apply (const cat) (cat (list (setf-> (str-alloc +reply_size)
					(+reply_node (task-nodeid))
					(+reply_system (system-id))
					(+reply_task_count (first stats))
					(+reply_idle (if timed (elem-get stats 4) 0))
					(+reply_time (if timed (pii-time) 0))
					(+reply_num_links (length links)))) links)))))))
