(report-header "TCP link: two networks joined over a real connection, as two machines are")

;this changes the network, so it is in tests/solo/, and runs on its own.
;Every other test of the links is of shared memory, or of the parts of the
;Net service one at a time. A fault in what goes over a TCP link itself, a
;ping the two ends did not agree the size of, passed them all, and no
;machine saw another. This is two networks and a connection between them.

(import "lib/task/pipe.inc")
(import "lib/net/links.inc")
(import "lib/crypto/random.inc")
(import "service/net/app.inc")

;a machine with no key has no links. One that has never been installed,
;a tree of cross built images say, is given one for the test, and the
;Net service a moment to see it
(defq tl_made_key (not (pii-fstat "mesh_key")))
(when tl_made_key
	(save (cat (to-lower (hex-encode (random-bytes 32))) (ascii-char 10)) "mesh_key")
	(task-sleep 3000000))

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
	;the links over the network are dropped, as the Net service does when
	;the key of the machine is another. A link is told it is closed till
	;there is none to tell
	;a link is the node's that made it, the one the Net service is on
	(defq tl_said (mail-mbox) tl_net_node (slice (net-service) +mailbox_id_size -1))
	(defun tl-drop ()
		(open-task (str `(progn (ffi "service/net/lisp_drop" net-drop)
				(mail-send (hex-decode ,(hex-encode tl_said)) (str (net-drop)))))
			tl_net_node +kn_call_pin 0 (mail-mbox))
		(str-as-num (ifn (mail-read-timeout tl_said (task-timeout 2)) "-1")))
	(assert-true "there is a link over the network to drop" (> (tl-drop) 0))
	(setq tl_t0 (pii-time))
	(while (and (/= (tl-drop) 0) (< (- (pii-time) tl_t0) (task-timeout 5)))
		(task-sleep 100000))
	(assert-eq "dropped, there is none" 0 (tl-drop))
	(while (and (find tl_new (lisp-nodes)) (< (- (pii-time) tl_t0) (task-timeout 15)))
		(task-sleep 100000))
	(assert-true "and the node at its other end is not seen any more" (not (find tl_new (lisp-nodes))))
	;the other end sees it is alone, and goes
	(while (and (pii-alive tl_pid) (< (- (pii-time) tl_t0) (task-timeout 25)))
		(task-sleep 10000))
	(assert-eq "left alone, its process ends" :nil (pii-alive tl_pid)))

;first come first served: one listener has a port, a second is told it
;has not, and when the first is told to go the port is there to have
(import "service/net/door.inc")
(defun tl-gate (port)
	; -> (gate has_port)
	(defq said (mail-mbox) gate (open-child "service/net/gate.lisp" +kn_call_pin))
	(mail-send gate (cat (door-key) (str port) " " (hex-encode said)))
	(list gate (mail-read-timeout said (task-timeout 5))))
(bind '(tl_gate1 tl_has1) (tl-gate 34582))
(bind '(tl_gate2 tl_has2) (tl-gate 34582))
(assert-eq "the first to listen on a port has it" "1" tl_has1)
(assert-eq "the second has not" "0" tl_has2)
(mail-send tl_gate1 "")
;it looks for mail between looks at the port, so it is asked for till it is had
(defq tl_t0 (pii-time) tl_gate3 :nil tl_has3 :nil)
(while (and (not (eql tl_has3 "1")) (< (- (pii-time) tl_t0) (task-timeout 5)))
	(task-sleep 20000)
	(bind '(tl_gate3 tl_has3) (tl-gate 34582)))
(assert-eq "the first told to go, the next has it" "1" tl_has3)
(mail-send tl_gate3 "")
(task-sleep 200000)

(when tl_made_key (pii-remove "mesh_key"))
