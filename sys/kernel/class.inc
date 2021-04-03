(include "sys/mail/class.inc")
(include "sys/kernel/lisp.inc")

(def-class sys_kernel nil
	(dec-method :id sys/kernel/id :static nil (r0 r1))
	(dec-method :kernel sys/kernel/kernel :static (r0))
	(dec-method :ping sys/kernel/ping)
	(dec-method :lisp_stats sys/kernel/lisp_stats :static (r0 r1) (r0 r1)))

(defcvar 'ping_period 5000000)
