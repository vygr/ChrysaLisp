(include "././list/class.inc")
(include "./struct.inc")

(def-class :hset :list
	(dec-method :vtable class/hset/vtable)
	(dec-method :type class/hset/type :override)
	;a set is itself and no other, whatever the two hold. A list is the
	;same as another by what is in it, and this is not a list to those
	;who use it
	(dec-method :eql class/obj/eql :final)
	(dec-method :create class/hset/create :static (:r0) (:r0))
	(dec-method :init class/hset/init :static (:r0 :r1 :r2))
	(dec-method :bucket class/hset/bucket :static (:r0 :r1) (:r0 :r2))
	(dec-method :cfind class/hset/cfind :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :cinsert class/hset/cinsert :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :each class/hset/each :static (:r0 :r1 :r2) (:r0))
	(dec-method :flush class/hset/flush :static (:r0) (:r0))
	(dec-method :each_callback class/obj/null :static (:r0 :r1 :r2)))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defun hset-buckets (this num iter_begin iter_end saved_this)
	;emit the buckets of an hset, or of an hmap on it, after the list's
	;init: of num_buckets more than 1, that many lists, its length that;
	;num is :r1 of the init, ok as it was given, 0 if the list's init
	;failed, and is this at the end, it went well
	(vpif `(,num /= 0))
		(assign `((,this +array_num_buckets)) `(,num))
		(vpif `(,num /= 1))
			(assign `(,num) `((,this +array_length)))
			(call :hset :set_cap `(,this ,num) `(,this) :r2)
			(class/array/get_both this iter_begin iter_end)
			(vp-cpy-rr this saved_this)
			(loop-start)
				(call :list :create :nil `((,iter_begin 0)))
				(vp-add-cr +ptr_size iter_begin)
			(loop-until `(,iter_begin = ,iter_end))
			(vp-cpy-rr saved_this this)
			;it went well: said here, and not left as whatever the last
			;create of a list had in the register
			(vp-cpy-rr this num)
		(endif)
	(endif))
