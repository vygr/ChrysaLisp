;one for each desktop, as a phone has one phone app: the call is the voice
;service's, and two of it would record and play over each other. Each
;desktop is a node, the app is on its node
(if (notany (# (eql (slice (hex-decode (second (split %0 ","))) +mailbox_id_size -1)
		(slice (task-mbox) +mailbox_id_size -1))) (mail-enquire "Voice,"))
	(import "./app_impl.lisp"))
