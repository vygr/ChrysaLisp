;one for each node that asks, which is each desktop. There was one for the
;machine, on the node of whichever desktop came up first, and it went when
;that desktop quit
(if (notany (# (eql (slice (hex-decode (second (split %0 ","))) +mailbox_id_size -1)
		(slice (task-mbox) +mailbox_id_size -1))) (mail-enquire "@Clipboard,"))
	(import "./app_impl.lisp"))
