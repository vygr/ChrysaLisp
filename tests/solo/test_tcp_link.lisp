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

(defq tl_before (lisp-nodes :t) tl_reply (mail-mbox) tl_new :nil)
(pipe-run "link -l 34581" (const identity))
(defq tl_pid (pii-spawn "-run tests/solo/tcp_peer.lisp"))

(defun tl-find ()
	;the node that is the process started, :nil if it is not seen yet
	(some (lambda (node)
			(unless (find node tl_before)
				(open-task (str `(mail-send (hex-decode ,(hex-encode tl_reply)) (str (pii-pid))))
					node +kn_call_pin 0 (mail-mbox))
				(if (eql (mail-read tl_reply) (str tl_pid)) node)))
		(lisp-nodes :t)))

(assert-true "the other network is started" (> tl_pid 0))
;each wait below is for what is to come, as long as it takes, a hang is
;the runner's to say. The other network ending before it is seen is what
;is to come too: it is said, with what it wrote of its link, and what this
;node has, so a failure says why
(until (or (setq tl_new (tl-find)) (not (pii-alive tl_pid)))
	(task-sleep 100000))
(assert-true (if tl_new "its node is seen over the connection, and runs a task"
		(cat "its node is seen over the connection, and runs a task. Not seen, it ended."
			" It said: " (ifn (load (cat "/tmp/tcp_peer_" (str tl_pid) ".log")) "nothing")
			" This node's links: " (str (length (net-links)))
			", nodes of this machine: " (str (length (lisp-nodes :t)))
			", this node: " (hex-encode (task-nodeid))))
	tl_new)
(when tl_new
	;a link whose other end is not yet known has a peer of nothing, all 0,
	;which (str-alloc) is not: it does not clear what it gives
	(defq tl_none (apply (const cat) (map (lambda (_) (char 0 +long_size)) (range 0 (/ +node_id_size +long_size)))))
	(assert-true "this node has a link whose other end is that node, or one on the way to it"
		(some (# (nql (getf %0 +link_peer_node) tl_none)) (net-links)))
	;mail too big for one packet, there and back, and it is what was sent.
	;A mailbox of its own, an answer to the search above may yet come late
	(defq tl_big (apply (const cat) (map (# (str %0 " ")) (range 0 30000))) tl_echo (mail-mbox))
	(open-task (str `(progn
			(defq mbox (mail-mbox))
			(mail-send (hex-decode ,(hex-encode tl_echo)) mbox)
			(mail-send (hex-decode ,(hex-encode tl_echo)) (mail-read mbox))))
		tl_new +kn_call_pin 0 (mail-mbox))
	(defq tl_there (mail-read tl_echo))
	(assert-true "a task there gives a mailbox" tl_there)
	(when tl_there
		(mail-send tl_there tl_big)
		(defq tl_back (mail-read tl_echo))
		(assert-eq "170K of mail goes there and comes back, all of it" (length tl_big) (length (ifn tl_back "")))
		(assert-true "and the same" (eql tl_big tl_back)))
	;the link stays up, a ping goes each way every second. Were they not
	;taken, the node would be dropped
	(task-sleep (task-timeout 4))
	(assert-true "four seconds on it is still seen" (find tl_new (lisp-nodes :t)))
	;the links over the network are dropped, as the Net service does when
	;the key of the machine is another. A link is told it is closed till
	;there is none to tell
	;a link is the node's that made it, the one the Net service is on
	(defq tl_said (mail-mbox) tl_net_node (slice (net-service) +mailbox_id_size -1))
	(defun tl-drop ()
		; (tl-drop) -> (count links)
		;how many links over the network were told to close, and of each
		;link the node has, its peer, the peer's task count, and 1 if it is
		;over the network
		(open-task (str `(progn (ffi "service/net/lisp_drop" net-drop) (ffi "service/net/lisp_links" net-links)
				(mail-send (hex-decode ,(hex-encode tl_said))
					(str (list (net-drop) (map (const hex-encode) (net-links)))))))
			tl_net_node +kn_call_pin 0 (mail-mbox))
		(bind '(n links) (first (read (string-stream (mail-read tl_said)))))
		(list n (map (lambda (link)
				(setq link (hex-decode link))
				(list (hex-encode (slice link 0 8)) (getf link +link_peer_tasks) (getf link +link_remote)))
			links)))
	(assert-true "there is a link over the network to drop" (> (first (tl-drop)) 0))
	;as long as it takes; each 5 seconds it is not done, what is left is said
	(defq tl_tries 0)
	(while (/= (first (defq tl_left (tl-drop))) 0)
		(if (= (% (++ tl_tries) 50) 0)
			(test-note (cat "still " (str (first tl_left)) " to drop, the links: " (str (second tl_left)))))
		(task-sleep 100000))
	(assert-eq "dropped, there is none" 0 (first (tl-drop)))
	(while (find tl_new (lisp-nodes :t))
		(task-sleep 100000))
	(assert-true "and the node at its other end is not seen any more" (not (find tl_new (lisp-nodes :t))))
	;the other end sees it is alone, and goes
	(while (pii-alive tl_pid)
		(task-sleep 10000))
	(assert-eq "left alone, its process ends" :nil (pii-alive tl_pid)))
;what the other network wrote of its link, kept only for a failure to say
(pii-remove (cat "/tmp/tcp_peer_" (str tl_pid) ".log"))

;first come first served: one listener has a port, a second is told it
;has not, and when the first is told to go the port is there to have
(import "service/net/door.inc")
(defun tl-gate (port)
	; -> (gate has_port)
	(defq said (mail-mbox) gate (open-child "service/net/gate.lisp" +kn_call_pin))
	(mail-send gate (cat (door-key) (str port) " " (hex-encode said)))
	(list gate (mail-read said)))
(bind '(tl_gate1 tl_has1) (tl-gate 34582))
(bind '(tl_gate2 tl_has2) (tl-gate 34582))
(assert-eq "the first to listen on a port has it" "1" tl_has1)
(assert-eq "the second has not" "0" tl_has2)
(mail-send tl_gate1 "")
;it looks for mail between looks at the port, so it is asked for till it is had
(defq tl_gate3 :nil tl_has3 :nil)
(until (eql tl_has3 "1")
	(task-sleep 20000)
	(bind '(tl_gate3 tl_has3) (tl-gate 34582)))
(assert-eq "the first told to go, the next has it" "1" tl_has3)
(mail-send tl_gate3 "")
(task-sleep 200000)

(when tl_made_key (pii-remove "mesh_key"))
