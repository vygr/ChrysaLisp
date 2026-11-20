(report-header "Hand back: a desktop that goes tells the member to take the mesh port back")

;the take port is the machine's, so this is in tests/solo/, and runs on its
;own. A member of this machine that gave its port to a desktop listens on
;it: if one does now, the port is taken, and this leaves it be

(import "service/net/app.inc")
(ffi "service/net/lisp_udp_bind" hb-bind)
(ffi "service/net/lisp_udp_recv" hb-recv)
(ffi "service/net/lisp_close" hb-close)

(cond
	((<= (defq hb_socket (hb-bind +mesh_take_port)) 0)
		(test-skip "a desktop that goes says the port is free" "a member of this machine waits on the take port now"))
	(:t (assert-eq "hand back is said" :t (mesh-hand-back))
		;it comes at once, over the loopback, it is waited for a little while
		(defq hb_got :nil hb_t0 (pii-time))
		(until (or hb_got (> (- (pii-time) hb_t0) 2000000))
			(unless (setq hb_got (hb-recv hb_socket 64)) (task-sleep 10000)))
		(assert-eq "a desktop that goes says the port is free" "TAKE" (if hb_got (first hb_got)))
		(hb-close hb_socket)))
