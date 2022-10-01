(include "sys/mail/class.inc")

(def-class sys_kernel :nil
	(dec-method :id sys/kernel/id :static :nil (:r0 :r1))
	(dec-method :kernel sys/kernel/kernel :static (:r0))
	(dec-method :ping sys/kernel/ping)
	(dec-method :lisp_stats sys/kernel/lisp_stats :static (:r0 :r1) (:r0 :r1)))

(defcvar 'ping_period 5000000)

;;;;;;;;;;;;;;;;;;;;;;;
; kernel mail functions
;;;;;;;;;;;;;;;;;;;;;;;

(def-enum kn_call 0
	(enum callback open child ping))

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

(def-struct kn_msg 0
	(ulong key)
	(struct reply_id net_id_size)
	(offset reply_size)
	(ulong func))

(def-struct kn_msg_callback kn_msg_size
	(ptr addr))

(def-struct kn_msg_open kn_msg_size
	(offset pathname))

(def-struct kn_msg_ping kn_msg_size
	(struct via node_id_size)
	(struct origin node_id_size)
	(uint session hops)
	(offset services))
