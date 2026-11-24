(include "././array/class.inc")
(include "./struct.inc")

;an array of raw bytes, 1 byte each: an element is its bits, read as a :num
;not sign extended, written its low bits; no maths, convert to :nums for
;that. Its storage methods are :array's made at its width
(def-class :bytes :array
	(dec-method :vtable class/bytes/vtable)
	(dec-method :type class/bytes/type :override)
	(dec-method :create class/bytes/create)
	(dec-method :vcreate class/bytes/create :final)
	(dec-method :set_cap class/bytes/set_cap :final)
	(dec-method :ref_elem class/bytes/ref_elem :final)
	(dec-method :ref_back class/bytes/ref_back :final)
	(dec-method :set_elem class/bytes/set_elem :final)
	(dec-method :fill class/bytes/fill :final)
	(dec-method :find class/bytes/find :final)
	(dec-method :rfind class/bytes/rfind :final)
	(dec-method :slice class/bytes/slice :final)
	(dec-method :cat class/bytes/cat :final)
	(dec-method :splice class/bytes/splice :final)
	(dec-method :append class/bytes/append :final)
	(dec-method :erase class/bytes/erase :final)
	(dec-method :erase2 class/bytes/erase2 :final)
	(dec-method :eql class/bytes/eql :final)
	(dec-method :hash class/bytes/hash :final)
	(dec-method :print class/bytes/print :final)
	(dec-method :push class/bytes/push :final)
	(dec-method :lisp_bytes class/bytes/lisp_bytes :static (:r0 :r1) (:r0 :r1)))
