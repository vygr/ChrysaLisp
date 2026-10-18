;one for each node that asks, which is each desktop, as the Audio service
;is: it has that desktop's microphone and speaker
(if (notany (# (eql (slice (hex-decode (second (split %0 ","))) +mailbox_id_size -1)
		(slice (task-mbox) +mailbox_id_size -1))) (mail-enquire "@Voice,"))
	(import "./app_impl.lisp"))
