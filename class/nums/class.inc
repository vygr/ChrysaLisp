(include "././array/class.inc")

(def-class nums array
	(dec-method :vtable class/nums/vtable)
	(dec-method :hash class/nums/hash :override)
	(dec-method :vcreate class/nums/create :override)
	(dec-method :add class/nums/add :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :sub class/nums/sub :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :mul class/nums/mul :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :div class/nums/div :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :mod class/nums/mod :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :abs class/nums/abs :virtual (:r0 :r1) (:r0))
	(dec-method :sum class/nums/sum :virtual (:r0) (:r0 :r1))
	(dec-method :scale class/nums/scale :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :min class/nums/min :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :max class/nums/max :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :dot class/nums/dot :virtual (:r0 :r1) (:r0 :r1))
	(dec-method :create class/nums/create)
	(dec-method :lisp_vecop1 class/nums/lisp_vecop1 :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :lisp_vecop2 class/nums/lisp_vecop2 :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :lisp_add class/nums/lisp_add :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_sub class/nums/lisp_sub :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_mul class/nums/lisp_mul :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_div class/nums/lisp_div :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_mod class/nums/lisp_mod :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_sum class/nums/lisp_sum :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_dot class/nums/lisp_dot :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_scale class/nums/lisp_scale :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_min class/nums/lisp_min :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_max class/nums/lisp_max :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_abs class/nums/lisp_abs :static (:r0 :r1) (:r0 :r1)))

(def-struct nums array_size)

;;;;;;;;;;;;;;;;;;;;;
; inline vector loops
;;;;;;;;;;;;;;;;;;;;;

(defun vec-loop-until (srcs elms src_end dst func)
	(loop-start)
		(each (# (assign (list (list %0 0)) (list %1))) srcs elms)
		(each (# (vp-add-cr +long_size %0)) srcs)
		(defq r (apply func elms))
		(when dst
			(assign (list r) (list (list dst 0)))
			(vp-add-cr +long_size dst))
	(loop-until `(,(first srcs) = ,src_end)))

(defun vec-loop-while (srcs elms src_end dst func)
	(loop-while `(,(first srcs) /= ,src_end))
		(each (# (assign (list (list %0 0)) (list %1))) srcs elms)
		(each (# (vp-add-cr +long_size %0)) srcs)
		(defq r (apply func elms))
		(when dst
			(assign (list r) (list (list dst 0)))
			(vp-add-cr +long_size dst))
	(loop-end))

(defun vec-loop-while-endswitch (srcs elms src_end func)
	(loop-while `(,(first srcs) /= ,src_end))
		(each (# (assign (list (list %0 0)) (list %1))) srcs elms)
		(each (# (vp-add-cr +long_size %0)) srcs)
		(apply func elms)
	(endswitch))
