(include "././seq/class.inc")
(include "./struct.inc")

(def-class :array :seq
	(dec-method :vtable class/array/vtable)
	(dec-method :type class/array/type :override)
	(dec-method :create class/array/create :static :nil (:r0))
	(dec-method :init class/array/init)
	(dec-method :deinit class/array/deinit :override)
	(dec-method :print class/array/print :override)
	(dec-method :eql class/array/eql :override)
	(dec-method :ref_elem class/array/ref_elem :override)
	(dec-method :slice class/array/slice :override)
	(dec-method :cat class/array/cat :override)
	(dec-method :find class/array/find :override)
	(dec-method :rfind class/array/rfind :override)
	(dec-method :splice class/array/splice :override)
	(dec-method :get_length class/array/get_length :final)
	(dec-method :vcreate class/array/create :virtual (:r0) (:r0))
	(dec-method :velement class/num/create :virtual (:r0) (:r0))
	(dec-method :clear class/array/clear :virtual (:r0) (:r0))
	(dec-method :truncate class/array/truncate :virtual (:r0 :r1) (:r0))
	(dec-method :ref_back class/array/ref_back :virtual (:r0) (:r0 :r1))
	(dec-method :set_elem class/array/set_elem :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :append class/array/append :virtual (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :erase class/array/erase :virtual (:r0 :r1) (:r0 :r1))
	(dec-method :erase2 class/array/erase2 :virtual (:r0 :r1) (:r0 :r1))
	(dec-method :fill class/array/fill :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :push class/array/push :virtual (:r0 :r1) (:r0 :r1))
	(dec-method :get_cap class/array/get_cap :static (:r0) (:r0 :r1))
	(dec-method :set_cap class/array/set_cap :virtual (:r0 :r1) (:r0))
	(dec-method :set_length class/array/set_length :static (:r0 :r1) (:r0 :r1))
	(dec-method :sort class/array/sort :static (:r0 :r1 :r2 :r3 :r4 :r5) (:r0))
	(dec-method :pivot class/array/pivot :static (:r0 :r1 :r2 :r3 :r4) (:r0 :r1))
	(dec-method :get_first class/array/get_first :static (:r0) (:r0 :r1))
	(dec-method :get_first2 class/array/get_first2 :static (:r0) (:r0 :r1 :r2))
	(dec-method :get_second class/array/get_second :static (:r0) (:r0 :r1))
	(dec-method :get_elem class/array/get_elem :static (:r0 :r1) (:r0 :r1))
	(dec-method :get_elem2 class/array/get_elem2 :static (:r0 :r1) (:r0 :r1 :r2))
	(dec-method :push_back class/array/push_back :static (:r0 :r1) (:r0 :r1 :r2 :r3))
	(dec-method :push_back2 class/array/push_back2 :static (:r0 :r1 :r2) (:r0 :r1 :r2 :r3 :r4))
	(dec-method :pop_back class/array/pop_back :static (:r0) (:r0 :r1))
	(dec-method :pop_back2 class/array/pop_back2 :static (:r0) (:r0 :r1 :r2))
	(dec-method :get_iter class/array/get_iter :static (:r0 :r1) (:r0 :r1))
	(dec-method :get_iters class/array/get_iters :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :get_begin class/array/get_begin :static (:r0) (:r0 :r1))
	(dec-method :get_end class/array/get_end :static (:r0) (:r0 :r1))
	(dec-method :get_both class/array/get_both :static (:r0) (:r0 :r1 :r2))
	(dec-method :sort_callback class/obj/null :static (:r0 :r1 :r2) (:r0))
	(dec-method :lisp_nums class/array/lisp_nums :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_fixeds class/array/lisp_fixeds :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_doubles class/array/lisp_doubles :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_path class/array/lisp_path :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_set class/array/lisp_eset :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_clear class/array/lisp_clear :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_push class/array/lisp_push :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_pop class/array/lisp_pop :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_cap class/array/lisp_cap :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_fill class/array/lisp_fill :static (:r0 :r1) (:r0 :r1)))

;;;;;;;;;;;;;;;;;;;
; parameter helpers
;;;;;;;;;;;;;;;;;;;

(defun array-get-args (i dst &optional type)
	;inputs
	;iter
	;(reg ...)
	;outputs
	;load register list from iter
	(cond
		((eql type :nil)
			(assign (map (lambda (&ignore) (list i (* +long_size (!)))) dst) dst))
		((defq s (type-to-size type))
			(assign (map (lambda (&ignore) (list i (* s (!)) type)) dst) dst))))

(defun array-bind-args (a dst &optional i)
	;inputs
	;array object
	;(reg ...)
	;outputs
	;load register list from array
	(setd i (last dst))
	(assign `((,a +array_begin)) `(,i))
	(array-get-args i dst))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defun class/array/init ()
	;inputs
	;:r0 = array object (ptr)
	;:r1 = vtable (pptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = 0 if error, else ok
	;trashes
	;:r1-:r2
	(vp-xor-rr :r2 :r2)
	(assign `(:r2 ,(>> (- +array_size +array_elems) (log2 +long_size))) '((:r0 +array_length) :r2))
	(assign '(:r2) '((:r0 +array_capacity)))
	(vp-lea-rcr :r0 +array_elems :r2)
	(assign '(:r2) '((:r0 +array_begin)))
	(s-call :array :init '(:r0 :r1) '(:r0 :r1)))

(defun class/array/get_cap ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = capacity (uint)
	;trashes
	;:r1
	(assign '((:r0 +array_capacity)) '(:r1)))

(defun class/array/get_length ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = length (uint)
	;trashes
	;:r1
	(assign '((:r0 +array_length)) '(:r1)))

(defun class/array/set_length ()
	;inputs
	;:r0 = array object (ptr)
	;:r1 = length (uint)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = length (uint)
	;trashes
	;none
	(assign '(:r1) '((:r0 +array_length))))

(defun class/array/get_begin ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = begin element iter (plong)
	;trashes
	;:r1
	(assign '((:r0 +array_begin)) '(:r1)))

(defun class/array/get_first ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = element (long)
	;trashes
	;:r1
	(assign '((:r0 +array_begin)) '(:r1))
	(assign '((:r1 0)) '(:r1)))

(defun class/array/get_second ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = element (long)
	;trashes
	;:r1
	(assign '((:r0 +array_begin)) '(:r1))
	(assign '((:r1 +long_size)) '(:r1)))

(defun class/array/get_iter (&optional o ei b)
	;inputs
	;:r0 = array object (ptr)
	;:r1 = element index (uint)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = element iter (plong)
	;trashes
	;:r1-:r2
	(setd o :r0 ei :r1 b :r2)
	(if (or (eql o ei) (eql b ei))
		(throw "Invalid object, index or base !" (list o ei b)))
	(assign `((,o +array_begin)) `(,b))
	(vp-shl-cr (log2 +long_size) ei)
	(vp-add-rr b ei))

(defun class/array/get_end (&optional o e b)
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = end element iter (plong)
	;trashes
	;:r1-:r2
	(setd o :r0 e :r1 b :r2)
	(if (eql o e)
		(throw "Invalid object, end or base !" (list o e b)))
	(assign `((,o +array_length)) `(,e))
	(class/array/get_iter o e b))

(defun class/array/get_both (&optional o b e)
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = begin element iter (plong)
	;:r2 = end element iter (plong)
	;trashes
	;:r1-:r2
	(setd o :r0 b :r1 e :r2)
	(if (or (eql o e) (eql b e))
		(throw "Invalid object, begin or end !" (list o b e)))
	(assign `((,o +array_length) (,o +array_begin)) `(,e ,b))
	(vp-shl-cr (log2 +long_size) e)
	(vp-add-rr b e))

(defun class/array/get_iters (&optional o bi ei b w)
	;inputs
	;:r0 = array object (ptr)
	;:r1 = begin index (uint)
	;:r2 = end index (uint)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = begin element iter (plong)
	;:r2 = end element iter (plong)
	;trashes
	;:r1-:r3
	;w the width of an element, a long by default
	(setd o :r0 bi :r1 ei :r2 b :r3 w +long_size)
	(if (or (eql o bi) (eql o ei) (eql bi ei) (eql b bi) (eql b ei))
		(throw "Invalid object, begin, end or base !" (list o bi ei b)))
	(assign `((,o +array_begin)) `(,b))
	(vp-shl-cr (log2 w) bi)
	(vp-shl-cr (log2 w) ei)
	(vp-add-rr b bi)
	(vp-add-rr b ei))

(defun class/array/get_elem (&optional o ei e b)
	;inputs
	;:r0 = array object (ptr)
	;:r1 = element index (uint)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = element (long)
	;trashes
	;:r1-:r2
	(setd o :r0 ei :r1 e :r1 b :r2)
	(if (or (eql o ei) (eql b ei))
		(throw "Invalid object, index, element or base !" (list o ei e b)))
	(assign `((,o +array_begin)) `(,b))
	(vp-shl-cr (log2 +long_size) ei)
	(vp-load-rrr b ei e))

(defun class/array/set_elem ()
	;inputs
	;:r0 = array object (ptr)
	;:r1 = element (long)
	;:r2 = element index (uint)
	;outputs
	;:r0 = array object (ptr)
	;trashes
	;:r2-:r3
	(assign '((:r0 +array_begin)) '(:r3))
	(vp-shl-cr (log2 +long_size) :r2)
	(vp-store-rrr :r1 :r3 :r2))

(defun class/array/pop_back ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = element (long)
	;trashes
	;:r1-:r2
	(assign '((:r0 +array_length) (:r0 +array_begin)) '(:r1 :r2))
	(vp-sub-cr 1 :r1)
	(assign '(:r1) '((:r0 +array_length)))
	(vp-shl-cr (log2 +long_size) :r1)
	(vp-load-rrr :r2 :r1 :r1))

(defun class/array/pop_back2 ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = element1 (long)
	;:r2 = element2 (long)
	;trashes
	;:r1-:r2
	(assign '((:r0 +array_length) (:r0 +array_begin)) '(:r1 :r2))
	(vp-sub-cr 2 :r1)
	(assign '(:r1) '((:r0 +array_length)))
	(vp-shl-cr (log2 +long_size) :r1)
	(vp-add-rr :r1 :r2)
	(array-get-args :r2 '(:r1 :r2)))

(defun class/array/clear ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;trashes
	;:r1
	(vp-xor-rr :r1 :r1)
	(assign '(:r1) '((:r0 +array_length))))

(defun class/array/get_first2 ()
	;inputs
	;:r0 = array object (ptr)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = element1 (long)
	;:r2 = element2 (long)
	;trashes
	;:r1-:r2
	(array-bind-args :r0 '(:r1 :r2)))

(defun class/array/get_elem2 ()
	;inputs
	;:r0 = array object (ptr)
	;:r1 = element index (uint)
	;outputs
	;:r0 = array object (ptr)
	;:r1 = element1 (long)
	;:r2 = element2 (long)
	;trashes
	;:r1-:r2
	(assign '((:r0 +array_begin)) '(:r2))
	(vp-shl-cr (log2 +long_size) :r1)
	(vp-add-rr :r1 :r2)
	(array-get-args :r2 '(:r1 :r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; methods by width: the body of a method of an array whose elements are w
; bytes, made in the def-method of each class that has its own, the width
; known here and nowhere else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-at (w b o)
	;the place of an element of w bytes at b + o, to be read, a long or
	;unsigned
	(if (= w +long_size) (list b o) (list b o (elem-get '(:nil ub us :nil ui) w))))

(defun array-to (w b o)
	;the place of an element of w bytes at b + o, to be written
	(if (= w +long_size) (list b o) (list b o (elem-get '(:nil b s :nil i) w))))

(defun array-load (w b o d)
	;an element of w bytes at b + o into d, a long or unsigned
	(if (= w +long_size)
		(vp-load-rcr b o d)
		(assign `((,b ,o ,(elem-get '(:nil ub us :nil ui) w))) `(,d))))

(defun array-store (w s b o)
	;s into an element of w bytes at b + o
	(if (= w +long_size)
		(vp-store-rrc s b o)
		(assign `(,s) `((,b ,o ,(elem-get '(:nil b s :nil i) w))))))

(defun array-slice (cls w)
	;:slice, a new array of the same class of the elements from si to ei,
	;reversed if ei is before si
	(vp-rdef (src si ei this di total) '(:r6 :r7 :r8))
	(entry cls :slice `(,src ,si ,ei))
	(call cls :vcreate `(,src) `(,this) :r1)
	(vpif `(,si /= ,ei))
		;first set the capacity/length
		(vp-cpy-rr si total)
		(vp-sub-rr ei total)
		(vp-abs-rr total total)
		(assign `(,total) `((,this +array_length)))
		(call cls :set_cap `(,this ,total) `(,this) :r2)
		;now transfer the slice, reversed if needed
		(class/array/get_iters src si ei total w)
		(assign `((,this +array_begin)) `(,di))
		(vpif `(,si < ,ei))
			(loop-start)
				(array-load w si 0 total)
				(vp-simd vp-add-cr `(,w) `(,si ,di))
				(array-store w total di (neg w))
			(loop-until `(,si = ,ei))
		(else)
			(loop-start)
				(array-load w si (neg w) total)
				(vp-simd vp-add-cr `(,(neg w) ,w) `(,si ,di))
				(array-store w total di (neg w))
			(loop-until `(,si = ,ei))
		(endif)
	(endif)
	(exit cls :slice `(,this))
	(vp-ret))

(defun array-ref-elem (cls w)
	;:ref_elem, the element at idx made the class's element object
	(vp-rdef (this idx base saved_this))
	(entry cls :ref_elem `(,this ,idx))
	(vp-cpy-rr this saved_this)
	(assign `((,this +array_begin)) `(,base))
	(vp-shl-cr (log2 w) idx)
	(assign (list (array-at w base idx)) `(,this))
	(v-call cls :velement `(,this) `(,this) saved_this idx)
	(exit cls :ref_elem `(,saved_this ,this))
	(vp-ret))

(defun array-ref-back (cls w)
	;:ref_back, the last element taken off, made the element object
	(vp-rdef (this len begin saved_this))
	(entry cls :ref_back `(,this))
	(assign `((,this +array_length) (,this +array_begin)) `(,len ,begin))
	(vp-sub-cr 1 len)
	(assign `(,len) `((,this +array_length)))
	(vp-shl-cr (log2 w) len)
	(vp-cpy-rr this saved_this)
	(assign (list (array-at w begin len)) `(,this))
	(v-call cls :velement `(,this) `(,this) saved_this len)
	(exit cls :ref_back `(,saved_this ,this))
	(vp-ret))

(defun array-set-elem (cls w)
	;:set_elem, :r1 into the element at :r2
	(entry cls :set_elem '(:r0 :r1 :r2))
	(assign '((:r0 +array_begin)) '(:r3))
	(vp-shl-cr (log2 w) :r2)
	(assign '(:r1) (list (array-to w :r3 :r2)))
	(exit cls :set_elem '(:r0))
	(vp-ret))

(defun array-fill (cls w)
	;:fill, a length of one element
	(entry cls :fill '(:r0 :r3 :r4))
	(call cls :set_cap '(:r0 :r3) '(:r0) :r2)
	(assign '(:r3) '((:r0 +array_length)))
	(assign '((:r0 +array_begin)) '(:r1))
	(vp-shl-cr (log2 w) :r3)
	(vp-lea-rrr :r1 :r3 :r2)
	(vpif '(:r1 /= :r2))
		(loop-start)
			(assign '(:r4) (list (array-to w :r1 0)))
			(vp-add-cr w :r1)
		(loop-until '(:r1 = :r2))
	(endif)
	(exit cls :fill '(:r0))
	(vp-ret))

(defun array-find (cls w)
	;:find, the index of elm from idx on, or -1
	(vp-rdef (this elm idx base tmp))
	(entry cls :find `(,this ,elm ,idx))
	(assign `((,this +array_length) (,this +array_begin)) `(,tmp ,base))
	(vp-simd vp-shl-cr `(,(log2 w)) `(,idx ,tmp))
	(vp-add-rr base idx)
	(vp-add-rr tmp base)
	(vpif `(,idx /= ,base))
		(loop-start)
			(assign (list (array-at w idx 0)) `(,tmp))
			(vp-add-cr w idx)
			(gotoif `(,elm = ,tmp) 'found)
		(loop-until `(,idx = ,base))
	(endif)
	(exit cls :find `(,this ,elm -1))
	(vp-ret)
(vp-label 'found)
	(assign `((,this +array_begin)) `(,base))
	(vp-sub-cr w idx)
	(vp-sub-rr base idx)
	(vp-shr-cr (log2 w) idx)
	(exit cls :find `(,this ,elm ,idx))
	(vp-ret))

(defun array-rfind (cls w)
	;:rfind, the index of elm from idx back, or -1
	(vp-rdef (this elm idx base tmp))
	(entry cls :rfind `(,this ,elm ,idx))
	(assign `((,this +array_begin)) `(,base))
	(vp-shl-cr (log2 w) idx)
	(vp-add-rr base idx)
	(vpif `(,idx /= ,base))
		(loop-start)
			(assign (list (array-at w idx (neg w))) `(,tmp))
			(breakif `(,elm = ,tmp))
			(vp-sub-cr w idx)
		(loop-until `(,idx = ,base))
	(endif)
	(vp-sub-rr base idx)
	(vp-asr-cr (log2 w) idx)
	(exit cls :rfind `(,this ,elm ,idx))
	(vp-ret))

(defun array-eql (cls w)
	;:eql, of the same class, length and elements
	(vp-rdef (this that len1 len2 itr1 itr2))
	(entry cls :eql `(,this ,that))
	(gotoif `(,this = ,that) 'same)
	(assign `((,this +obj_vtable) (,that +obj_vtable)) `(,len1 ,len2))
	(gotoif `(,len1 /= ,len2) 'not_same)
	(assign `((,this +array_length) (,that +array_length)) `(,len1 ,len2))
	(gotoif `(,len1 /= ,len2) 'not_same)
	(gotoif `(,len1 = 0) 'same)
	(assign `((,this +array_begin) (,that +array_begin)) `(,itr1 ,itr2))
	(vp-shl-cr (log2 w) len1)
	(vp-add-rr itr1 len1)
	(loop-start)
		(assign (list (array-at w itr1 0) (array-at w itr2 0)) `(,that ,len2))
		(gotoif `(,that /= ,len2) 'not_same)
		(vp-simd vp-add-cr `(,w) `(,itr1 ,itr2))
	(loop-until `(,itr1 = ,len1))
(vp-label 'same)
	(exit cls :eql `(,this 0))
	(vp-ret)
(vp-label 'not_same)
	(exit cls :eql `(,this -1))
	(vp-ret))

(defun array-erase (cls w)
	;:erase, the element at iter, the last put in its place
	(vp-rdef (this iter len begin))
	(entry cls :erase `(,this ,iter))
	(assign `((,this +array_length) (,this +array_begin)) `(,len ,begin))
	(vp-sub-cr 1 len)
	(assign `(,len) `((,this +array_length)))
	(vp-shl-cr (log2 w) len)
	(vp-add-rr len begin)
	(vpif `(,begin /= ,iter))
		(assign (list (array-at w begin 0)) `(,len))
		(assign `(,len) (list (array-to w iter 0)))
	(endif)
	(exit cls :erase `(,this ,iter))
	(vp-ret))

(defun array-erase2 (cls w)
	;:erase2, the two elements at iter, the last two put in their place
	(vp-rdef (this iter len begin))
	(entry cls :erase2 `(,this ,iter))
	(assign `((,this +array_length) (,this +array_begin)) `(,len ,begin))
	(vp-sub-cr 2 len)
	(assign `(,len) `((,this +array_length)))
	(vp-shl-cr (log2 w) len)
	(vp-add-rr len begin)
	(vpif `(,begin /= ,iter))
		(assign (list (array-at w begin 0) (array-at w begin w)) `(,begin ,len))
		(assign `(,begin ,len) (list (array-to w iter 0) (array-to w iter w)))
	(endif)
	(exit cls :erase2 `(,this ,iter))
	(vp-ret))

(defun array-cat (cls w)
	;:cat, a new array of the same class of the elements of each of seqs
	(vp-rdef (this seqs_begin di src si ei
			len total seqs seqs_end))
	(entry cls :cat `(,this ,seqs))
	(call cls :vcreate `(,this) `(,this) :r1)
	(class/array/get_both seqs seqs_begin seqs_end)
	(vpif `(,seqs_begin /= ,seqs_end))
		(vp-xor-rr total total)
		(loop-start)
			(assign `((,seqs_begin 0)) `(,src))
			(vp-add-cr +ptr_size seqs_begin)
			(assign `((,src +array_length)) `(,len))
			(vp-add-rr len total)
		(loop-until `(,seqs_begin = ,seqs_end))
		(assign `(,total) `((,this +array_length)))
		(call cls :set_cap `(,this ,total) `(,this) :r2)
		(assign `((,this +array_begin) (,seqs +array_begin)) `(,di ,seqs_begin))
		(loop-start)
			(assign `((,seqs_begin 0)) `(,src))
			(vp-add-cr +ptr_size seqs_begin)
			(assign `((,src +array_length) (,src +array_begin)) `(,ei ,si))
			(vp-shl-cr (log2 w) ei)
			(vp-add-rr si ei)
			(vpif `(,si /= ,ei))
				(loop-start)
					(assign (list (array-at w si 0)) `(,len))
					(vp-simd vp-add-cr `(,w) `(,si ,di))
					(assign `(,len) (list (array-to w di (neg w))))
				(loop-until `(,si = ,ei))
			(endif)
		(loop-until `(,seqs_begin = ,seqs_end))
	(endif)
	(exit cls :cat `(,this))
	(vp-ret))

(defun array-append (cls w)
	;:append, the elements of that from si to ei on the end
	(vp-rdef (saved_that saved_si saved_ei di this that si ei)
		'(:r6 :r7 :r8 :r9))
	(entry cls :append `(,this ,that ,si ,ei))
	(vpif `(,si /= ,ei))
		(assign `(,that ,si ,ei) `(,saved_that ,saved_si ,saved_ei))
		(assign `((,this +array_length)) `(,di))
		(vp-sub-rr si ei)
		(assign `((& ,ei ,di)) `(,that))
		(assign `(,that) `((,this +array_length)))
		(call cls :set_cap `(,this ,that) `(,this) :r2)
		(assign `((,saved_that +array_begin)) `(,that))
		(vp-simd vp-shl-cr `(,(log2 w)) `(,saved_si ,saved_ei ,di))
		(vp-simd vp-add-rr `(,that) `(,saved_si ,saved_ei))
		(assign `((,this +array_begin)) `(,that))
		(vp-add-rr that di)
		(loop-start)
			(assign (list (array-at w saved_si 0)) `(,that))
			(vp-simd vp-add-cr `(,w) `(,saved_si ,di))
			(assign `(,that) (list (array-to w di (neg w))))
		(loop-until `(,saved_si = ,saved_ei))
	(endif)
	(exit cls :append `(,this))
	(vp-ret))

(defun array-splice (cls w)
	;:splice, a new array of the same class of the slices of src1 and src2
	;by the pairs of idxs, alternating
	(vp-rdef (this idx_begin di si ei swp_xor len total
			src1 src2 idxs idx_end))
	(entry cls :splice `(,src1 ,src2 ,idxs))
	(call cls :vcreate `(,src1) `(,this) :r1)
	(class/array/get_both idxs idx_begin idx_end)
	(vpif `(,idx_begin /= ,idx_end))
		(assign `((,src1 +array_length) (,src2 +array_length)) `(,len ,ei))
		(vp-cpy-rr len swp_xor)
		(vp-xor-rr ei swp_xor)
		(vp-xor-rr total total)
		(loop-start)
			(vp-simd vp-load-rcr `(,idx_begin) `(0 ,+long_size) `(,si ,ei))
			(vp-add-cr (* +long_size 2) idx_begin)
			(seq-elem-index si len)
			(seq-elem-index ei len)
			(vp-xor-rr swp_xor len)
			(vp-sub-rr si ei)
			(vp-abs-rr ei ei)
			(vp-add-rr ei total)
		(loop-until `(,idx_begin = ,idx_end))
		(assign `(,total) `((,this +array_length)))
		(call cls :set_cap `(,this ,total) `(,this) :r2)
		(assign `((,this +array_begin) (,idxs +array_begin)) `(,di ,idx_begin))
		(vp-cpy-rr src1 swp_xor)
		(vp-xor-rr src2 swp_xor)
		(loop-start)
			(vp-simd vp-load-rcr `(,idx_begin) `(0 ,+long_size) `(,si ,ei))
			(vp-add-cr (* +long_size 2) idx_begin)
			(assign `((,src1 +array_length) (,src1 +array_begin)) `(,len ,src2))
			(seq-elem-index si len)
			(seq-elem-index ei len)
			(vp-xor-rr swp_xor src1)
			(vpif `(,si /= ,ei))
				(vp-simd vp-shl-cr `(,(log2 w)) `(,si ,ei))
				(vp-simd vp-add-rr `(,src2) `(,si ,ei))
				(vpif `(,si < ,ei))
					(loop-start)
						(assign (list (array-at w si 0)) `(,len))
						(vp-simd vp-add-cr `(,w) `(,si ,di))
						(assign `(,len) (list (array-to w di (neg w))))
					(loop-until `(,si = ,ei))
				(else)
					(loop-start)
						(assign (list (array-at w si (neg w))) `(,len))
						(vp-simd vp-add-cr `(,(neg w) ,w) `(,si ,di))
						(assign `(,len) (list (array-to w di (neg w))))
					(loop-until `(,si = ,ei))
				(endif)
			(endif)
		(loop-until `(,idx_begin = ,idx_end))
	(endif)
	(exit cls :splice `(,this))
	(vp-ret))

(defun array-set-cap (cls w)
	;:set_cap of an array of elements narrower than a long: the capacity
	;of that many elements in longs, rounded up, by the set_cap of the
	;class it is built on, of longs
	(entry cls :set_cap '(:r0 :r1))
	(vp-add-cr (dec (/ +long_size w)) :r1)
	(vp-shr-cr (log2 (/ +long_size w)) :r1)
	(s-jump cls :set_cap '(:r0 :r1)))

(defun array-hash (cls w)
	;:hash, the elements xored, raw
	(vp-rdef (src val src1 src_end e0))
	(entry cls :hash `(,src))
	(vp-xor-rr val val)
	(assign `((,src +array_length) (,src +array_begin)) `(,src_end ,src1))
	(vp-shl-cr (log2 w) src_end)
	(vp-add-rr src1 src_end)
	(vpif `(,src1 /= ,src_end))
		(loop-start)
			(assign (list (array-at w src1 0)) `(,e0))
			(vp-add-cr w src1)
			(vp-xor-rr e0 val)
		(loop-until `(,src1 = ,src_end))
	(endif)
	(vp-shr-cr 1 val)
	(exit cls :hash `(,src ,val))
	(vp-ret))

(defun array-print (cls w)
	;:print, each element made its element object and printed
	(structure slot 0
		(ptr this stream)
		(pint iter_begin iter_end))
	(vp-alloc slot_size)
	(entry cls :print '(:r0 :r1))
	(save-fields :rsp '(slot_this slot_stream) '(:r0 :r1))
	(call :stream :write_char '(:r1 +char_lrb))
	(assign '((:rsp slot_this)) '(:r0))
	(assign '((:r0 +array_length) (:r0 +array_begin)) '(:r2 :r4))
	(vp-shl-cr (log2 w) :r2)
	(vp-add-rr :r4 :r2)
	(vpif '(:r4 /= :r2))
		(save-fields :rsp '(slot_iter_begin slot_iter_end) '(:r4 :r2))
		(loop-start)
			(assign (list (array-at w :r4 0)) '(:r0))
			(assign '((:rsp slot_this)) '(:r1))
			(v-call cls :velement '(:r0) '(:r0) :r1 :r1)
			(call :num :print '(:r0 (:rsp slot_stream)) '(:r0) :r2)
			(call :num :deref '(:r0))
			(load-fields :rsp '(slot_iter_begin slot_iter_end) '(:r4 :r2))
			(vp-add-cr w :r4)
			(breakif '(:r4 = :r2))
			(assign '(:r4) '((:rsp slot_iter_begin)))
			(call :stream :write_char '((:rsp slot_stream) +char_space))
		(loop-end)
	(endif)
	(call :stream :write_char '((:rsp slot_stream) +char_rrb))
	(exit cls :print '((:rsp slot_this)))
	(vp-free slot_size)
	(vp-ret))

;the arrays of elements narrower than a long, and the width of each: a
;push of one into another takes its bytes by it
(defq +array_narrow ''((:bytes 1) (:shorts 2) (:ints 4) (:floats 4)))

(defun array-push (cls w)
	;:push, each of the args after the first onto the end, raw as push has
	;them: a number the low w bytes of its value, one of the same class
	;its elements, any other array or a str its bytes, w to an element
	(vp-rdef (this arg tmp len) '(:r0 :r1 :r2 :r3))
	;not :r10, which lisp_push keeps over this
	(vp-rdef (saved iter iter_end) '(:r11 :r12 :r13))
	(entry cls :push `(,this ,arg))
	(vp-cpy-rr this saved)
	(class/array/get_both arg iter iter_end)
	(vp-add-cr +ptr_size iter)
	(loop-while `(,iter /= ,iter_end))
		(assign `((,iter 0)) `(,this))
		(vp-add-cr +ptr_size iter)
		(switch)
			(call :obj :inst_of `(,this (@ ,(f-path :num :vtable))) `(,arg ,tmp))
			(nextcaseif `(,tmp = 0))
			;a number, the low w bytes of its value
			(assign `((,arg +num_value) (,saved +array_length)) `(,tmp ,len))
			(vp-push tmp len)
			(vp-cpy-rr len arg)
			(vp-add-cr 1 arg)
			(assign `(,arg) `((,saved +array_length)))
			(call cls :set_cap `(,saved ,arg) :nil :r2)
			(vp-pop tmp len)
			(assign `((,saved +array_begin)) `(,arg))
			(vp-shl-cr (log2 w) len)
			(assign `(,tmp) (list (array-to w arg len)))
			(break)
		(default)
			(call :obj :inst_of `(,arg (@ ,(f-path cls :vtable))) `(,arg ,tmp))
			(nextcaseif `(,tmp = 0))
			;one of the same class, its elements
			(assign `((,arg +array_length)) `(,len))
			(call cls :append `(,saved ,arg 0 ,len) :nil :r6)
			(break)
		;one of another width, its bytes
		(each (lambda ((kind kw))
				(unless (eql kind cls)
					(default)
						(call :obj :inst_of `(,arg (@ ,(f-path kind :vtable))) `(,arg ,tmp))
						(nextcaseif `(,tmp = 0))
						(assign `((,arg +array_length) (,arg +array_begin)) `(,len ,tmp))
						(if (> kw 1) (vp-shl-cr (log2 kw) len))
						(goto 'bytes)))
			+array_narrow)
		(default)
			(call :obj :inst_of `(,arg (@ ,(f-path :array :vtable))) `(,arg ,tmp))
			(nextcaseif `(,tmp = 0))
			;another array, of longs, its bytes
			(assign `((,arg +array_length) (,arg +array_begin)) `(,len ,tmp))
			(vp-shl-cr (log2 +long_size) len)
			(goto 'bytes)
		(default)
			(call :obj :inst_of `(,arg (@ ,(f-path :str :vtable))) `(,arg ,tmp))
			(nextcaseif `(,tmp = 0))
			;a str, its bytes
			(assign `((,arg +str_length)) `(,len))
			(vp-lea-rcr arg +str_data tmp)
		(vp-label 'bytes)
			;len bytes at tmp, the whole elements of them
			(vp-shr-cr (log2 w) len)
			(assign `((,saved +array_length)) `(,arg))
			(vp-push tmp len arg)
			(vp-add-rr len arg)
			(assign `(,arg) `((,saved +array_length)))
			(call cls :set_cap `(,saved ,arg) :nil :r2)
			(vp-pop tmp len arg)
			(assign `((,saved +array_begin)) `(,this))
			(vp-shl-cr (log2 w) arg)
			(vp-add-rr this arg)
			(vp-shl-cr (log2 w) len)
			(call :sys_mem :copy `(,tmp ,arg ,len))
			(break)
		(default)
			(goto 'error)
		(endswitch)
	(loop-end)
	(exit cls :push `(,saved -1))
	(vp-ret)
(vp-label 'error)
	(exit cls :push `(,saved 0))
	(vp-ret))

