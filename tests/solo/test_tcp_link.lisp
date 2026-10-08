(report-header "TCP link: two networks joined over a real connection, as two machines are")

;this changes the network, so it is in tests/solo/, and runs on its own.
;Every other test of the links is of shared memory, or of the parts of the
;Net service one at a time. A fault in what goes over a TCP link itself, a
;ping the two ends did not agree the size of, passed them all, and no
;machine saw another. This is two networks and a connection between them.

(import "lib/task/pipe.inc")
(import "lib/net/links.inc")

(defq tl_before (lisp-nodes) tl_reply (mail-mbox) tl_new :nil)
(pipe-run "link -l 34581" (const identity))
(defq tl_pid (pii-spawn "-run tests/solo/tcp_peer.lisp") tl_t0 (pii-time))

(defun tl-find ()
	;the node that is the process started, :nil if it is not seen yet
	(some (lambda (node)
			(unless (find node tl_before)
				(open-task (str `(mail-send (hex-decode ,(hex-encode tl_reply)) (str (pii-pid))))
					node +kn_call_pin 0 (mail-mbox))
				(if (eql (mail-read-timeout tl_reply (task-timeout 1)) (str tl_pid)) node)))
		(lisp-nodes)))

(assert-true "the other network is started" (> tl_pid 0))
(while (and (not (setq tl_new (tl-find))) (< (- (pii-time) tl_t0) (task-timeout 15)))
	(task-sleep 100000))
(assert-true "its node is seen over the connection, and runs a task" tl_new)
(when tl_new
	(assert-true "this node has a link whose other end is that node, or one on the way to it"
		(some (# (nql (getf %0 +link_peer_node) (const (str-alloc +node_id_size)))) (net-links)))
	;mail too big for one packet, there and back, and it is what was sent.
	;A mailbox of its own, an answer to the search above may yet come late
	(defq tl_big (apply (const cat) (map (# (str %0 " ")) (range 0 30000))) tl_echo (mail-mbox))
	(open-task (str `(progn
			(defq mbox (mail-mbox))
			(mail-send (hex-decode ,(hex-encode tl_echo)) mbox)
			(mail-send (hex-decode ,(hex-encode tl_echo)) (mail-read mbox))))
		tl_new +kn_call_pin 0 (mail-mbox))
	(defq tl_there (mail-read-timeout tl_echo (task-timeout 5)))
	(assert-true "a task there gives a mailbox" tl_there)
	(when tl_there
		(mail-send tl_there tl_big)
		(defq tl_back (mail-read-timeout tl_echo (task-timeout 10)))
		(assert-eq "170K of mail goes there and comes back, all of it" (length tl_big) (length (ifn tl_back "")))
		(assert-true "and the same" (eql tl_big tl_back)))
	;the link stays up, a ping goes each way every second. Were they not
	;taken, the node would be dropped
	(task-sleep (task-timeout 4))
	(assert-true "four seconds on it is still seen" (find tl_new (lisp-nodes)))
	(open-task "(pii-exit)" tl_new +kn_call_pin 0 (mail-mbox))
	(setq tl_t0 (pii-time))
	(while (and (pii-alive tl_pid) (< (- (pii-time) tl_t0) (task-timeout 5)))
		(task-sleep 10000))
	(assert-eq "told to exit, its process ends" :nil (pii-alive tl_pid)))
