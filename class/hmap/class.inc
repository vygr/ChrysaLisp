(include "././list/class.inc")
(include "././hset/class.inc")
(include "./struct.inc")

(def-class :hmap :list
	(dec-method :vtable class/hmap/vtable)
	(dec-method :type class/hmap/type :override)
	;a map is itself and no other, whatever the two hold. A list is the
	;same as another by what is in it, and this is not a list to those
	;who use it
	(dec-method :eql class/obj/eql :final)
	(dec-method :deinit class/hmap/deinit :override)
	(dec-method :create class/hmap/create :static (:r0) (:r0))
	(dec-method :init class/hmap/init :static (:r0 :r1 :r2))
	(dec-method :cfind class/hmap/cfind :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :pfind class/hmap/pfind :static (:r0 :r1) (:r0 :r1 :r2))
	(dec-method :pinsert class/hmap/pinsert :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :each class/hmap/each :static (:r0 :r1 :r2) (:r0))
	(dec-method :copy class/hmap/copy :static (:r0 :r1) (:r0 :r1))
	(dec-method :resize class/hmap/resize :static (:r0 :r1) (:r0))
	(dec-method :list class/hmap/list :static (:r0) (:r0 :r1))
	(dec-method :search class/hmap/search :static (:r0 :r1) (:r0 :r1 :r2))
	(dec-method :set class/hmap/set :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :get class/hmap/get :static (:r0 :r1) (:r0 :r1))
	(dec-method :set_parent class/hmap/set_parent :static (:r0 :r1) (:r0))
	(dec-method :each_callback class/obj/null :static (:r0 :r1 :r2))
	(dec-method :lisp_env class/hmap/lisp_env :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_def class/hmap/lisp_def :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_defq class/hmap/lisp_defq :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_set class/hmap/lisp_set :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_setq class/hmap/lisp_setq :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_get class/hmap/lisp_get :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_defx class/hmap/lisp_defx :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_undef class/hmap/lisp_undef :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_parent class/hmap/lisp_penv :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_list class/hmap/lisp_tolist :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_resize class/hmap/lisp_resize :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_copy class/hmap/lisp_copy :static (:r0 :r1) (:r0 :r1)))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defun hmap-search (this key iter bucket)
	;emit the search of an hmap, and then of its parents, for a key, as
	;:hmap :pfind and :hset :bucket do it, but in line, it is the hot path
	;of every symbol lookup. Falls out the end not found, with iter 0, and
	;jumps to the 'found label if it is.
	;trashes
	;:r3-:r7, and what :hset :bucket does, :r1-:r6
	(vp-rdef (iter_end iter_begin slot num saved_key) '(:r3 :r4 :r5 :r6 :r7))
	(assign `(,key) `(,saved_key))
	(loop-start)
		;most are a single bucket, the hmap itself
		(assign `((,this +array_num_buckets)) `(,num))
		(vp-cpy-rr this bucket)
		(vpif `(,num /= 1))
			(call :hset :bucket `(,this ,saved_key) `(,this ,bucket))
		(endif)
		(class/array/get_both bucket iter_begin iter_end)
		(vpif `(,iter_begin /= ,iter_end))
			;use the key address, and last known slot
			(vp-cpy-rr iter_begin iter)
			(assign `((,saved_key +str_hashslot)) `(,slot))
			(vp-shl-cr (log2 (* +ptr_size 2)) slot)
			(vp-add-rr slot iter)
			(vpif `(,iter < ,iter_end))
				(assign `((,iter 0)) `(,slot))
				(gotoif `(,saved_key = ,slot) 'found)
			(endif)
			;cache miss, so scan the bucket
			(vp-cpy-rr iter_begin iter)
			(loop-start)
				(assign `((,iter 0)) `(,slot))
				(gotoif `(,saved_key = ,slot) 'scan_found)
				(vp-add-cr (* +ptr_size 2) iter)
			(loop-until `(,iter = ,iter_end))
		(endif)
		;not here, so on to the parent
		(assign `((,this +hmap_parent)) `(,this))
	(loop-until `(,this = 0))
	(vp-xor-rr iter iter)
	(goto 'not_found)
(vp-label 'scan_found)
	;note the slot for next time
	(vp-cpy-rr iter slot)
	(vp-sub-rr iter_begin slot)
	(vp-shr-cr (log2 (* +ptr_size 2)) slot)
	(assign `(,slot) `((,saved_key +str_hashslot))))
