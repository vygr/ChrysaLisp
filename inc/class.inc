;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

(defcfun class-name (n) (sym-cat "_class_" (str n)))
(defcfun super-name (n) (sym-cat "_super_" (str n)))

(defcvar '*class* nil (class-name 'null) '())

(defcfun vtable-emit (_1 &optional _2)
	;_1 = class name
	;_2 = parent class name
	(if _2 (fn-add-link _2) (vp-long 0))
	(each (lambda (m)
		(if (or (eql (elem 2 m) 'virtual) (eql (elem 2 m) 'final))
			(fn-add-link (elem 1 m)))) (eval (class-name _1))))

(defcfun def-class (_1 &optional _2)
	;_1 = class name
	;_2 = super class name
	(setq _2 (opt _2 'null) *class* _1)
	(defcvar (super-name _1) _2 (class-name _1) (cat (eval (class-name _2)))))

(defcfun def-method (_1 _2 &optional _3 _4 _5)
	;_1 = member name
	;_2 = member function
	;_3 = mode (static/virtual/final/override)
	;_4 = in regs
	;_5 = out regs
	(setd _3 'static)
	(defq s 0 c (eval (class-name *class*))
		m (some (lambda (m)
			(if (eql (elem 0 m) _1) m
				(progn (setq s (inc s)) nil))) c))
	(defun override ()
		(setq m (cat m))
		(elem-set s c m)
		(elem-set 1 m _2)
		(if _4 (elem-set 3 m _4))
		(if _5 (elem-set 4 m _5)))
	(if m
		(cond
			;existing method
			((eql _3 'static)
				(if (eql (elem 2 m) 'static)
					(override)
					(error "Virtual method already exists !" _1)))
			((eql _3 'virtual)
				(error "Method already exists !, use override ?" _1))
			((eql _3 'override)
				(cond
					((eql (elem 2 m) 'virtual)
						(override))
					((eql (elem 2 m) 'final)
						(error "Virtual method is final !" _1))
					(t (error "Static method already exists !" _1))))
			((eql _3 'final)
				(cond
					((eql (elem 2 m) 'virtual)
						(override)
						(elem-set 2 m _3))
					((eql (elem 2 m) 'final)
						(error "Virtual method is final !" _1))
					(t (error "Static method already exists !" _1))))
			(t (error "Must use static, virtual, final or override ! " _3)))
		(cond
			;new method
			((or (eql _3 'static) (eql _3 'virtual) (eql _3 'final))
				(push c (list _1 _2 _3 _4 _5)))
			((eql _3 'override)
				(error "Method does not exist !" _1))
			(t (error "Must use static, virtual, final or override ! " _3)))))

;;;;;;;;;;;;;;;
; class calling
;;;;;;;;;;;;;;;

(defmacro method-info (_1 _2)
	`(progn
		(defq s nil v nil m nil)
		(method-lookup ,_1 ,_2)))

(defcfun method-lookup (_1 _2)
	;_1 = class name
	;_2 = member name
	;writes to s, v, m in parent environment !
	(setq s 0 v ptr_size)
	(defq c (eval (class-name _1)))
	(until (or (eq s (length c)) (eql (elem 0 (setq m (elem s c))) _2))
		(if (or (eql (elem 2 m) 'virtual) (eql (elem 2 m) 'final))
			(setq v (add v ptr_size)))
		(setq s (inc s)))
	(if (eq s (length c))
		(error "No such method !" (list _1 _2))))

(defcfun f-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(method-info _1 _2)
	(elem 1 m))

(defcfun s-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(method-info (eval (super-name _1)) _2)
	(elem 1 m))

(defcfun f-entry (_1 _2 _3)
	;_1 = class name
	;_2 = slot method name
	;_3 = in parameters
	(method-info _1 _2)
	(assign (elem 3 m) _3))

(defcfun f-exit (_1 _2 _3)
	;_1 = class name
	;_2 = slot method name
	;_3 = out parameters
	(method-info _1 _2)
	(assign _3 (elem 4 m)))

;;;;;;;;;;;;;;;;
; method calling
;;;;;;;;;;;;;;;;

(defcfun f-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(method-info _1 _2)
	(if (eql (elem 2 m) 'virtual)
		(error "Method is virtual ! Use v-call ?" (list _1 _2)))
	(assign _3 (elem 3 m))
	(fn-call (elem 1 m))
	(if _4 (assign (elem 4 m) _4)))

(defcfun f-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info _1 _2)
	(if (eql (elem 2 m) 'virtual)
		(error "Method is virtual ! Use v-jmp ?" (list _1 _2)))
	(assign _3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun f-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info _1 _2)
	(if (eql (elem 2 m) 'virtual)
		(error "Method is virtual ! Use v-bind ?" (list _1 _2)))
	(fn-bind (elem 1 m) _3))

(defcfun s-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(method-info (eval (super-name _1)) _2)
	(if (eql (elem 2 m) 'static)
		(unless (eql (elem 0 m) 'init)
			(error "Method is static !" (list (eval (super-name _1)) _2))))
	(assign _3 (elem 3 m))
	(fn-call (elem 1 m))
	(if _4 (assign (elem 4 m) _4)))

(defcfun s-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info (eval (super-name _1)) _2)
	(if (eql (elem 2 m) 'static)
		(unless (eql (elem 0 m) 'init)
			(error "Method is static !" (list (eval (super-name _1)) _2))))
	(assign _3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun s-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info (eval (super-name _1)) _2)
	(if (eql (elem 2 m) 'static)
		(error "Method is static !" (list (eval (super-name _1)) _2)))
	(fn-bind (elem 1 m) _3))

(defcfun v-call (_1 _2 &optional _3 _4 _5)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	(setd _5 r15)
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-call ?" (list _1 _2)))
	(setq s (elem 3 m))
	(if (str? _3)
		(setq _3 (cat "RXX, " _3) s (cat (list _5) s)))
	(assign _3 s)
	(vp-cpy-ir r0 obj_vtable _5)
	(vp-call-i _5 v)
	(if _4 (assign (elem 4 m) _4)))

(defcfun v-jmp (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(setd _4 r15)
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-jmp ?" (list _1 _2)))
	(setq s (elem 3 m))
	(if (str? _3)
		(setq _3 (cat "RXX, " _3) s (cat (list _4) s)))
	(assign _3 s)
	(vp-cpy-ir r0 obj_vtable _4)
	(vp-jmp-i _4 v))

(defcfun v-bind (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = dispatch reg
	(setd _3 r15)
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-bind ?" (list _1 _2)))
	(vp-cpy-ir r0 obj_vtable _3)
	(vp-cpy-ir _3 v _3))

(defcfun d-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-call ?" (list _1 _2)))
	(assign _3 (elem 3 m))
	(fn-call (elem 1 m))
	(if _4 (assign (elem 4 m) _4)))

(defcfun d-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-jmp ?" (list _1 _2)))
	(assign _3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun d-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-bind ?" (list _1 _2)))
	(fn-bind (elem 1 m) _3))

(defcfun l-call (_1 &optional _2 _3 _4 _5)
	;_1 = label
	;_2 = in parameters
	;_3 = in regs
	;_4 = out parameters
	;_5 = out regs
	(assign _2 _3)
	(vp-call _1)
	(if _5 (assign _4 _5)))

;;;;;;;;;;;;;;;;;
; virtual classes
;;;;;;;;;;;;;;;;;

(def-class 'class)
(def-method 'obj 'class/class_obj)
(def-method 'ref 'class/class_ref)
(def-method 'component 'class/class_component)
(def-method 'string 'class/class_string)
(def-method 'vector 'class/class_vector)
(def-method 'pair 'class/class_pair)
(def-method 'unordered_set 'class/class_unordered_set)
(def-method 'unordered_map 'class/class_unordered_map)
(def-method 'stream 'class/class_stream)
(def-method 'stream_str 'class/class_stream_str)
(def-method 'stream_msg_out 'class/class_stream_msg_out)
(def-method 'stream_msg_in 'class/class_stream_msg_in)
(def-method 'text 'class/class_text)
(def-method 'view 'class/class_view)
(def-method 'label 'class/class_label)
(def-method 'button 'class/class_button)
(def-method 'flow 'class/class_flow)
(def-method 'grid 'class/class_grid)
(def-method 'window 'class/class_window)
(def-method 'progress 'class/class_progress)
(def-method 'title 'class/class_title)
(def-method 'slave 'class/class_slave)
(def-method 'master 'class/class_master)
(def-method 'boxed_ptr 'class/class_boxed_ptr)
(def-method 'boxed_long 'class/class_boxed_long)
(def-method 'lisp 'class/class_lisp)
(def-method 'symbol 'class/class_symbol)
(def-method 'sequence 'class/class_sequence)
(def-method 'sliceable 'class/class_sliceable)
(def-method 'error 'class/class_error)
(def-method 'vdu 'class/class_vdu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic class construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun gen-class (_1)
	;_1 = class name
	(def-func (f-path 'class _1))
		(defq c (eval (super-name _1)))
		(if (eql c 'null)
			(vtable-emit _1)
			(vtable-emit _1 (f-path 'class c)))
	(def-func-end))

(defcfun gen-create (_1 &optional _2)
	;_1 = class name
	;_2 = number of args
	(setd _2 0)
	(defq _3 (slice 0 _2 '(r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11))
		_4 (slice 0 _2 '(r14 r15 r5 r6 r7 r8 r9 r10 r11 r12 r13)))
	(def-func (sym-cat "class/" _1 "/create"))
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;all but r4
		(assign _3 _4)
		(f-call _1 'new '() '(r0))
		(vpif '(r0 != 0))
			;init the object
			(f-call _1 'init `(r0 (@ ,(f-path 'class _1)) ~_4) '(r0 r1))
			(vpif '(r1 == 0))
				;error with init
				(v-call _1 'delete '(r0) '() r1)
				(vp-xor-rr r0 r0)
			(endif)
		(endif)
		(vp-ret)
	(def-func-end))

(defcfun gen-new (_1)
	;_1 = class name
	(def-func (sym-cat "class/" _1 "/new"))
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3
		(f-jmp 'sys_mem 'alloc `(,(sym-cat _1 "_size")))
	(def-func-end))
