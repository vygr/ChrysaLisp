(include 'class/seq/class.inc)

(def-class 'array 'seq)
(dec-method 'vtable 'class/array/vtable)
(dec-method 'create 'class/array/create 'static nil '(r0))
(dec-method 'init 'class/array/init)
(dec-method 'get_capacity 'class/array/get_capacity 'static '(r0) '(r0 r1))
(dec-method 'set_capacity 'class/array/set_capacity 'static '(r0 r1) '(r0))
(dec-method 'set_length 'class/array/set_length 'static '(r0 r1) '(r0 r1))
(dec-method 'find 'class/array/find 'static '(r0 r1) '(r0 r1 r2))
(dec-method 'sort 'class/array/sort 'static '(r0 r1 r2 r3 r4 r5) '(r0))
(dec-method 'partition 'class/array/partition 'static '(r0 r1 r2 r3 r4) '(r0 r1))
(dec-method 'get_first 'class/array/get_first 'static '(r0) '(r0 r1))
(dec-method 'get_second 'class/array/get_second 'static '(r0) '(r0 r1))
(dec-method 'get_element 'class/array/get_element 'static '(r0 r1) '(r0 r1))
(dec-method 'push_back 'class/array/push_back 'static '(r0 r1) '(r0 r1 r2 r3))
(dec-method 'pop_back 'class/array/pop_back 'static '(r0) '(r0 r1))
(dec-method 'get_iter 'class/array/get_iter 'static '(r0 r1) '(r0 r1))
(dec-method 'get_iters 'class/array/get_iters 'static '(r0 r1 r2) '(r0 r1 r2))
(dec-method 'get_begin 'class/array/get_begin 'static '(r0) '(r0 r1))
(dec-method 'get_end 'class/array/get_end 'static '(r0) '(r0 r1))
(dec-method 'get_both 'class/array/get_both 'static '(r0) '(r0 r1 r2))
(dec-method 'sort_callback 'class/obj/null 'static '(r0 r1 r2) '(r0))

(dec-method 'clear 'class/array/clear 'virtual '(r0) '(r0))
(dec-method 'ref_back 'class/array/ref_back 'virtual '(r0) '(r0 r1))
(dec-method 'set_element 'class/array/set_element 'virtual '(r0 r1 r2) '(r0))
(dec-method 'append 'class/array/append 'virtual '(r0 r1 r2 r3) '(r0))

(dec-method 'deinit 'class/array/deinit 'override)
(dec-method 'get_length 'class/array/get_length 'override)
(dec-method 'ref_element 'class/array/ref_element 'override)
(dec-method 'slice 'class/array/slice 'override)
(dec-method 'cat 'class/array/cat 'override)

(dec-method 'lisp_array 'class/array/lisp_array 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_points 'class/array/lisp_points 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_clear 'class/array/lisp_clear 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_push 'class/array/lisp_push 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_pop 'class/array/lisp_pop 'static '(r0 r1) '(r0 r1))

(def-struct 'array 'seq)
	(pulong 'begin)
	(uint 'length)
	(uint 'capacity)
	(ulong 'e0)
	(ulong 'e1)
	(ulong 'e2)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/array/init ()
	;inputs
	;r0 = array object (ptr)
	;r1 = vtable (pptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = 0 if error, else ok
	;trashes
	;r2
	(vp-xor-rr r2 r2)
	(class/array/set_length r0 r2)
	(vp-cpy-cr (shr (sub array_size array_e0) (log2 long_size)) r2)
	(vp-cpy-ri-i r2 r0 array_capacity)
	(vp-lea-i r0 array_e0 r2)
	(vp-cpy-ri r2 r0 array_begin)
	(s-call 'array 'init '(r0 r1) '(r0 r1)))

(defcfun class/array/get_capacity (&optional o c)
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = capacity (uint)
	;trashes
	;r1
	(setd o r0 c r1)
	(vp-cpy-ir-ui o array_capacity c))

(defcfun-bind class/array/get_length (&optional o l)
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = length (uint)
	;trashes
	;r1
	(setd o r0 l r1)
	(vp-cpy-ir-ui o array_length l))

(defcfun class/array/set_length (&optional o l)
	;inputs
	;r0 = array object (ptr)
	;r1 = length (uint)
	;outputs
	;r0 = array object (ptr)
	;r1 = length (uint)
	;trashes
	;none
	(setd o r0 l r1)
	(if (eql o l)
		(throw "Invalid object or length !" (list o l)))
	(vp-cpy-ri-i l o array_length))

(defcfun-bind class/array/get_begin (&optional o b)
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = begin element iter (plong)
	;trashes
	;r1
	(setd o r0 b r1)
	(vp-cpy-ir o array_begin b))

(defcfun-bind class/array/get_first (&optional o e)
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	;trashes
	;r1
	(setd o r0 e r1)
	(class/array/get_begin o e)
	(vp-cpy-ir e 0 e))

(defcfun class/array/get_second ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	;trashes
	;r1
	(class/array/get_begin r0 r1)
	(vp-cpy-ir r1 long_size r1))

(defcfun-bind class/array/get_iter (&optional o ei b)
	;inputs
	;r0 = array object (ptr)
	;r1 = element index (uint)
	;outputs
	;r0 = array object (ptr)
	;r1 = element iter (plong)
	;trashes
	;r1-r2
	(setd o r0 ei r1 b r2)
	(if (or (eql o ei) (eql b ei))
		(throw "Invalid object, index or base !" (list o ei b)))
	(class/array/get_begin o b)
	(vp-shl-cr (log2 long_size) ei)
	(vp-add-rr b ei))

(defcfun class/array/get_end (&optional o e b)
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = end element iter (plong)
	;trashes
	;r1-r2
	(setd o r0 e r1 b r2)
	(if (eql o e)
		(throw "Invalid object, end or base !" (list o e b)))
	(class/array/get_length o e)
	(class/array/get_iter o e b))

(defcfun-bind class/array/get_both (&optional o b e)
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = begin element iter (plong)
	;r2 = end element iter (plong)
	;trashes
	;r1-r2
	(setd o r0 b r1 e r2)
	(if (or (eql o e) (eql b e))
		(throw "Invalid object, begin or end !" (list o b e)))
	(class/array/get_length o e)
	(class/array/get_begin o b)
	(vp-shl-cr (log2 long_size) e)
	(vp-add-rr b e))

(defcfun-bind class/array/get_iters (&optional o bi ei b)
	;inputs
	;r0 = array object (ptr)
	;r1 = begin index (uint)
	;r2 = end index (uint)
	;outputs
	;r0 = array object (ptr)
	;r1 = begin element iter (plong)
	;r2 = end element iter (plong)
	;trashes
	;r1-r3
	(setd o r0 bi r1 ei r2 b r3)
	(if (or (eql o bi) (eql o ei) (eql bi ei) (eql b bi) (eql b ei))
		(throw "Invalid object, begin, end or base !" (list o bi ei b)))
	(class/array/get_begin o b)
	(vp-shl-cr (log2 long_size) bi)
	(vp-shl-cr (log2 long_size) ei)
	(vp-add-rr b bi)
	(vp-add-rr b ei))

(defcfun-bind class/array/get_element (&optional o ei e b)
	;inputs
	;r0 = array object (ptr)
	;r1 = element index (uint)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	;trashes
	;r1-r2
	(setd o r0 ei r1 e r1 b r2)
	(if (or (eql o ei) (eql b ei))
		(throw "Invalid object, index, element or base !" (list o ei e b)))
	(class/array/get_begin o b)
	(vp-shl-cr (log2 long_size) ei)
	(vp-cpy-dr b ei e))

(defcfun class/array/set_element ()
	;inputs
	;r0 = array object (ptr)
	;r1 = element (long)
	;r2 = element index (uint)
	;outputs
	;r0 = array object (ptr)
	;trashes
	;r2-r3
	(class/array/get_begin r0 r3)
	(vp-shl-cr (log2 long_size) r2)
	(vp-cpy-rd r1 r3 r2))

(defcfun class/array/pop_back ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	;trashes
	;r1-r2
	(class/array/get_length r0 r1)
	(class/array/get_begin r0 r2)
	(vp-sub-cr 1 r1)
	(class/array/set_length r0 r1)
	(vp-shl-cr (log2 long_size) r1)
	(vp-cpy-dr r2 r1 r1))

(defcfun class/array/clear ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;trashes
	;r1
	(vp-xor-rr r1 r1)
	(class/array/set_length r0 r1))

;;;;;;;;;;;;;;;;;;
; paramater macros
;;;;;;;;;;;;;;;;;;

(defcfun-bind class/array/set_args (i regs)
	(assign-asm-to-asm regs (map (lambda (r) (list i (mul long_size _))) regs)))

(defcfun-bind class/array/get_args (i regs)
	(assign-asm-to-asm (map (lambda (r) (list i (mul long_size _))) regs) regs))

(defcfun-bind class/array/bind_args (a regs)
	(class/array/get_begin a (elem -2 regs))
	(class/array/get_args (elem -2 regs) regs))
