;the other end of tests/solo/test_tcp_link.lisp. A node started by that
;test, alone, a network of one. It links to the test's network over TCP,
;by the address of this machine itself, sealed as one over the
;internet is, and stays till it is told to go,
;or its link has gone and it is alone again, or a minute, so one that is
;forgotten does not stay for ever
;it keeps to itself: its Net service is not to join the mesh for the machine
(def *root_env* '*env_node_solo* :t)
(import "lib/task/pipe.inc")
(import "lib/net/links.inc")
;what it does is written down for the test, which says it if this ends
;before the test has seen it, /tmp/tcp_peer_<pid>.log
(defq tp_log (cat "/tmp/tcp_peer_" (str (pii-pid)) ".log") tp_said (list))
(pipe-run "link -s 127.0.0.1:34581" (# (push tp_said %0) (prin %0)))
(defq tp_t0 (pii-time) tp_linked :nil)
(save (cat "link said: " (join tp_said "") ";") tp_log)
(while (and (< (- (pii-time) tp_t0) (task-timeout 60))
		(or (not tp_linked) (nempty? (net-links))))
	(if (nempty? (net-links)) (setq tp_linked :t))
	(task-sleep 200000))
(save (cat "link said: " (join tp_said "") "; linked: " (if tp_linked "yes" "never")
	"; ended after " (str (/ (- (pii-time) tp_t0) 1000000)) "s") tp_log)
(pii-exit)
