(include "class/obj/class.inc")

(def-class dim obj
	(dec-method :vtable class/dim/vtable)
	(dec-method :create class/dim/create :static (:r0 :r1) (:r0))
	(dec-method :init class/dim/init :static (:r0 :r1 :r2 :r3))
	(dec-method :deinit class/dim/deinit :final)
	(dec-method :lisp_dim class/dim/lisp_dim :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_get class/dim/lisp_get :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_set class/dim/lisp_set :static (:r0 :r1) (:r0 :r1)))

(def-struct dim obj_size
	(ptr dims elems))
