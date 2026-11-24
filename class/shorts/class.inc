(include "././array/class.inc")
(include "./struct.inc")

;an array of raw shorts, 2 bytes each: an element is its bits, read as a :num
;not sign extended, written its low bits; no maths, convert to :nums for
;that. Its storage methods are :array's made at its width
(def-class :shorts :array
	(dec-method :vtable class/shorts/vtable)
	(dec-method :type class/shorts/type :override)
	(dec-method :create class/shorts/create)
	(dec-method :vcreate class/shorts/create :final)
	(dec-method :set_cap class/shorts/set_cap :final)
	(dec-method :ref_elem class/shorts/ref_elem :final)
	(dec-method :ref_back class/shorts/ref_back :final)
	(dec-method :set_elem class/shorts/set_elem :final)
	(dec-method :fill class/shorts/fill :final)
	(dec-method :find class/shorts/find :final)
	(dec-method :rfind class/shorts/rfind :final)
	(dec-method :slice class/shorts/slice :final)
	(dec-method :cat class/shorts/cat :final)
	(dec-method :splice class/shorts/splice :final)
	(dec-method :append class/shorts/append :final)
	(dec-method :erase class/shorts/erase :final)
	(dec-method :erase2 class/shorts/erase2 :final)
	(dec-method :eql class/shorts/eql :final)
	(dec-method :hash class/shorts/hash :final)
	(dec-method :print class/shorts/print :final)
	(dec-method :push class/shorts/push :final)
	(dec-method :lisp_shorts class/shorts/lisp_shorts :static (:r0 :r1) (:r0 :r1)))
