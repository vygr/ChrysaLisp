;one for each desktop, as a phone has one phone app: the call is the voice
;service's, and two of it would record and play over each other. Each
;desktop is a node, the app is on its node. One said to be here is asked:
;one that died without going said so and does not answer, and a new one
;is opened
(defq here (filter (# (eql (slice (hex-decode (second (split %0 ","))) +mailbox_id_size -1)
		(slice (task-mbox) +mailbox_id_size -1))) (mail-enquire "Voice,"))
	answer (mail-mbox))
(each (# (mail-send (hex-decode (second (split %0 ","))) (hex-encode answer))) here)
(if (or (empty? here) (not (mail-read-timeout answer 300000)))
	(import "./app_impl.lisp"))
