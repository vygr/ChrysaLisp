;;;;;;;;;;;;;;;;;;;;;;;
; service/net/class.inc
;;;;;;;;;;;;;;;;;;;;;;;
(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;;;;
; host net PII class
;;;;;;;;;;;;;;;;;;;;

(def-class :host_net :nil
	(dec-method :init :nil :virtual (abi-args 0) (:r0))
	(dec-method :deinit :nil :virtual (abi-args 0) (:r0))
	(dec-method :connect :nil :virtual (abi-args 2) (:r0))
	(dec-method :listen :nil :virtual (abi-args 1) (:r0))
	(dec-method :accept :nil :virtual (abi-args 1) (:r0))
	(dec-method :send :nil :virtual (abi-args 3) (:r0))
	(dec-method :recv :nil :virtual (abi-args 3) (:r0))
	(dec-method :close :nil :virtual (abi-args 1) (:r0))
	(dec-method :poll :nil :virtual (abi-args 1) (:r0))
	(dec-method :lisp_init service/net/lisp_init :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_deinit service/net/lisp_deinit :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_connect service/net/lisp_connect :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_listen service/net/lisp_listen :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_accept service/net/lisp_accept :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_send service/net/lisp_send :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_recv service/net/lisp_recv :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_close service/net/lisp_close :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_poll service/net/lisp_poll :static (:r0 :r1) (:r0 :r1)))

(defun host-net-call (%0 %1 &optional %2 %3 %4 %5)
	;%0 = class name (:host_net)
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	;%4 = dispatch reg
	;%5 = temp reg
	(setd %4 :r14 %5 :r13)
	(fn-bind 'sys/statics/statics %4)
	(vp-cpy-ir %4 statics_sys_load_host_net_funcs %4)
	(abi-call-table %0 %1 %4 %2 %3 %5))