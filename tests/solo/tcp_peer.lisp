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
(pipe-run "link -s 127.0.0.1:34581" (const prin))
(defq tp_t0 (pii-time) tp_linked :nil)
(while (and (< (- (pii-time) tp_t0) (task-timeout 60))
		(or (not tp_linked) (nempty? (net-links))))
	(if (nempty? (net-links)) (setq tp_linked :t))
	(task-sleep 200000))
(pii-exit)
