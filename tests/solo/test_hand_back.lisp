(report-header "Hand back: a desktop that goes tells the member to take the mesh port back")

;it is said on a port of this test's own, not the machine's take port: a
;bind may share a UDP port, so a member waiting there now would hear what
;this says, and this what a desktop that goes says to it. The port is
;fixed, so this is in tests/solo/, and runs on its own

(import "service/net/app.inc")
(ffi "service/net/lisp_udp_bind" hb-bind)
(ffi "service/net/lisp_udp_recv" hb-recv)
(ffi "service/net/lisp_close" hb-close)

(defq hb_port 33357)
(cond
	((<= (defq hb_socket (hb-bind hb_port)) 0)
		(test-skip "a desktop that goes says the port is free" "the port of this test is taken"))
	(:t (assert-eq "hand back is said" :t (mesh-hand-back hb_port))
		;it comes at once, over the loopback, it is waited for a little while
		(defq hb_got :nil hb_t0 (pii-time))
		(until (or hb_got (> (- (pii-time) hb_t0) 2000000))
			(unless (setq hb_got (hb-recv hb_socket 64)) (task-sleep 10000)))
		(assert-eq "a desktop that goes says the port is free" "TAKE" (if hb_got (first hb_got)))
		(hb-close hb_socket)))
