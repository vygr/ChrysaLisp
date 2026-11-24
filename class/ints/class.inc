(include "././array/class.inc")
(include "./struct.inc")

;an array of raw ints, 4 bytes each: an element is its bits, read as a :num
;not sign extended, written its low bits; no maths, convert to :nums for
;that. Its storage methods are :array's made at its width
(def-class :ints :array
	(dec-method :vtable class/ints/vtable)
	(dec-method :type class/ints/type :override)
	(dec-method :create class/ints/create)
	(dec-method :vcreate class/ints/create :final)
	(dec-method :set_cap class/ints/set_cap :final)
	(dec-method :ref_elem class/ints/ref_elem :final)
	(dec-method :ref_back class/ints/ref_back :final)
	(dec-method :set_elem class/ints/set_elem :final)
	(dec-method :fill class/ints/fill :final)
	(dec-method :find class/ints/find :final)
	(dec-method :rfind class/ints/rfind :final)
	(dec-method :slice class/ints/slice :final)
	(dec-method :cat class/ints/cat :final)
	(dec-method :splice class/ints/splice :final)
	(dec-method :append class/ints/append :final)
	(dec-method :erase class/ints/erase :final)
	(dec-method :erase2 class/ints/erase2 :final)
	(dec-method :eql class/ints/eql :final)
	(dec-method :hash class/ints/hash :final)
	(dec-method :print class/ints/print :final)
	(dec-method :push class/ints/push :final)
	(dec-method :lisp_ints class/ints/lisp_ints :static (:r0 :r1) (:r0 :r1)))
