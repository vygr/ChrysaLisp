(include "class/obj/class.inc")

(def-class stdio obj
	(dec-method :vtable class/stdio/vtable)
	(dec-method :create class/stdio/create :static :nil (:r0))
	(dec-method :init class/stdio/init :static (:r0 :r1))
	(dec-method :deinit class/stdio/deinit :final)
	(dec-method :lisp_create class/stdio/lisp_create :static (:r0 :r1) (:r0 :r1)))

(def-struct stdio obj_size
	(ptr stdin stdout stderr args))

(def-struct stdio_msg_init 0
	(struct stdout_id net_id_size)
	(offset reply)
	(struct stderr_id net_id_size)
	(struct ack_id net_id_size)
	(offset args))
