;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

(defmacro class-sym (_)
	`(sym-cat "_class_" ,_))

(defmacro super-sym (_)
	`(sym-cat "_super_" ,_))

(defcvar '*class* nil (class-sym 'null) '() '*args* (list))
(each (lambda (_) (push *args* (list))) (range 0 16))

(defcfun intern-args (l)
	(when l
		(defq _ (some! nil nil t (lambda (_)
			(and (match? _ l) _)) (list (defq b (elem (length l) *args*)))))
		(if _ _ (progn (push b l) l))))

(defcfun vtable-emit (_1)
	;_1 = class name
	(if (eql (defq s (eval (super-sym _1))) 'null)
		(vp-long 0)
		(fn-add-link (f-path 'class s)))
	(each (lambda (m)
		(if (or (eql (elem 2 m) 'virtual) (eql (elem 2 m) 'final))
			(fn-add-link (elem 1 m)))) (eval (class-sym _1))))

(defcfun def-class (_1 &optional _2)
	;_1 = class name
	;_2 = super class name
	(setq _2 (opt _2 'null) *class* _1)
	(defcvar (super-sym _1) _2 (class-sym _1) (cat (eval (class-sym _2)))))

(defcfun def-method (_1 _2 &optional _3 _4 _5)
	;_1 = member name
	;_2 = member function
	;_3 = mode (static/virtual/final/override)
	;_4 = in regs
	;_5 = out regs
	(setd _3 'static)
	(defq s 0 c (eval (class-sym *class*))
		m (some (lambda (m)
			(if (eql (elem 0 m) _1) m
				(progn (setq s (inc s)) nil))) c))
	(defun override ()
		(setq m (cat m))
		(elem-set s c m)
		(elem-set 1 m _2)
		(if _4 (elem-set 3 m (intern-args _4)))
		(if _5 (elem-set 4 m (intern-args _5))))
	(if m
		(cond
			;existing method
			((eql _3 'static)
				(if (eql (elem 2 m) 'static)
					(override)
					(throw "Virtual method already exists !" _1)))
			((eql _3 'virtual)
				(throw "Method already exists !, use override ?" _1))
			((eql _3 'override)
				(cond
					((eql (elem 2 m) 'virtual)
						(override))
					((eql (elem 2 m) 'final)
						(throw "Virtual method is final !" _1))
					(t (throw "Static method already exists !" _1))))
			((eql _3 'final)
				(cond
					((eql (elem 2 m) 'virtual)
						(override)
						(elem-set 2 m _3))
					((eql (elem 2 m) 'final)
						(throw "Virtual method is final !" _1))
					(t (throw "Static method already exists !" _1))))
			(t (throw "Must use static, virtual, final or override ! " _3)))
		(cond
			;new method
			((or (eql _3 'static) (eql _3 'virtual) (eql _3 'final))
				(push c (list _1 _2 _3 (intern-args _4) (intern-args _5))))
			((eql _3 'override)
				(throw "Method does not exist !" _1))
			(t (throw "Must use static, virtual, final or override ! " _3)))))

;;;;;;;;;;;;;;;
; class calling
;;;;;;;;;;;;;;;

(defmacro method-info (_1 _2)
	`(progn
		(defq s 0 v ptr_size m nil)
		(method-lookup ,_1 ,_2)))

(defcfun method-slot (_1 _2)
	;_1 = class name
	;_2 = member name
	(defq s 0 v ptr_size m nil)
	(method-lookup _1 _2) s)

(defcfun method-lookup (_1 _2)
	;_1 = class name
	;_2 = member name
	;writes to s, v, m in parent environment !
	(defq c (eval (class-sym _1)))
	(until (or (eq s (length c)) (eql (elem 0 (setq m (elem s c))) _2))
		(if (or (eql (elem 2 m) 'virtual) (eql (elem 2 m) 'final))
			(setq v (add v ptr_size)))
		(setq s (inc s)))
	(if (eq s (length c))
		(throw "No such method !" (list _1 _2))))

(defcfun f-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(method-info _1 _2)
	(elem 1 m))

(defcfun s-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(method-info (eval (super-sym _1)) _2)
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

(defcfun l-entry (_1)
	;_1 = in parameters
	(defq _ '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(cond
		((not _1))
		((str? _1) (assign (slice 0 (length (split _1 ",")) _) _1))
		(t (assign (slice 0 (length _1) _) _1))))

(defcfun l-exit (_1)
	;_1 = out parameters
	(defq _ '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(cond
		((not _1))
		((str? _1) (assign _1 (slice 0 (length (split _1 ",")) _)))
		(t (assign _1 (slice 0 (length _1) _)))))

;;;;;;;;;;;;;;;;
; method calling
;;;;;;;;;;;;;;;;

(defcfun f-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(method-info _1 _2)
	(if (not (eql (elem 2 m) 'static))
		(throw "Method is not static ! Use v-call ?" (list _1 _2)))
	(assign _3 (elem 3 m))
	(fn-call (elem 1 m))
	(if _4 (assign (elem 4 m) _4)))

(defcfun f-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info _1 _2)
	(if (not (eql (elem 2 m) 'static))
		(throw "Method is not static ! Use v-jmp ?" (list _1 _2)))
	(assign _3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun f-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info _1 _2)
	(if (not (eql (elem 2 m) 'static))
		(throw "Method is not static ! Use v-bind ?" (list _1 _2)))
	(fn-bind (elem 1 m) _3))

(defcfun s-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(method-info (eval (super-sym _1)) _2)
	(if (eql (elem 2 m) 'static)
		(unless (eql (elem 0 m) 'init)
			(throw "Method is static !" (list (eval (super-sym _1)) _2))))
	(assign _3 (elem 3 m))
	(fn-call (elem 1 m))
	(if _4 (assign (elem 4 m) _4)))

(defcfun s-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info (eval (super-sym _1)) _2)
	(if (eql (elem 2 m) 'static)
		(unless (eql (elem 0 m) 'init)
			(throw "Method is static !" (list (eval (super-sym _1)) _2))))
	(assign _3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun s-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info (eval (super-sym _1)) _2)
	(if (eql (elem 2 m) 'static)
		(throw "Method is static !" (list (eval (super-sym _1)) _2)))
	(fn-bind (elem 1 m) _3))

(defcfun v-call (_1 _2 &optional _3 _4 _5)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	(setd _5 r14)
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(throw "Method is static ! Use f-call ?" (list _1 _2)))
	(setq s (elem 3 m))
	(if (find _5 s)
		(throw "Dispatch register conflicts with arg !" (list _5 s)))
	(assign _3 s)
	(vp-cpy-ir r0 obj_vtable _5)
	(vp-call-i _5 v)
	(if _4 (assign (elem 4 m) _4)))

(defcfun v-jmp (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(setd _4 r14)
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(throw "Method is static ! Use f-jmp ?" (list _1 _2)))
	(setq s (elem 3 m))
	(if (find _4 s)
		(throw "Dispatch register conflicts with arg !" (list _4 s)))
	(assign _3 s)
	(vp-cpy-ir r0 obj_vtable _4)
	(vp-jmp-i _4 v))

(defcfun v-bind (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = dispatch reg
	(setd _3 r14)
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(throw "Method is static ! Use f-bind ?" (list _1 _2)))
	(vp-cpy-ir r0 obj_vtable _3)
	(vp-cpy-ir _3 v _3))

(defcfun d-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(throw "Method is static ! Use f-call ?" (list _1 _2)))
	(assign _3 (elem 3 m))
	(fn-call (elem 1 m))
	(if _4 (assign (elem 4 m) _4)))

(defcfun d-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(throw "Method is static ! Use f-jmp ?" (list _1 _2)))
	(assign _3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun d-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info _1 _2)
	(if (eql (elem 2 m) 'static)
		(throw "Method is static ! Use f-bind ?" (list _1 _2)))
	(fn-bind (elem 1 m) _3))

(defcfun r-call (_1 _2 &optional _3 _4 _5)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	(setd _5 r14)
	(method-info _1 _2)
	(setq s (elem 3 m))
	(if (find _5 s)
		(throw "Dispatch register conflicts with arg !" (list _5 s)))
	(assign _3 (cat s (list _5)))
	(vp-call-r _5)
	(if _4 (assign (elem 4 m) _4)))

(defcfun l-call (_1 &optional _2 _3)
	;_1 = label
	;_2 = in parameters
	;_3 = out parameters
	(defq _ '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(cond
		((not _2))
		((str? _2) (assign _2 (slice 0 (length (split _2 ",")) _)))
		(t (assign _2 (slice 0 (length _2) _))))
	(vp-call _1)
	(cond
		((not _3))
		((str? _3) (assign (slice 0 (length (split _3 ",")) _) _3))
		(t (assign (slice 0 (length _3) _) _3))))

;;;;;;;;;;;;;;;;;
; virtual classes
;;;;;;;;;;;;;;;;;

(def-class 'class)
(def-method 'obj 'class/class_obj)
(def-method 'component 'class/class_component)
(def-method 'string 'class/class_string)
(def-method 'array 'class/class_array)
(def-method 'vector 'class/class_vector)
(def-method 'pair 'class/class_pair)
(def-method 'hash_set 'class/class_hash_set)
(def-method 'hash_map 'class/class_hash_map)
(def-method 'stream 'class/class_stream)
(def-method 'stream_str 'class/class_stream_str)
(def-method 'msg_out 'class/class_msg_out)
(def-method 'msg_in 'class/class_msg_in)
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
(def-method 'pipe 'class/class_pipe)
(def-method 'function 'class/class_function)
(def-method 'integer 'class/class_integer)
(def-method 'lisp 'class/class_lisp)
(def-method 'symbol 'class/class_symbol)
(def-method 'sequence 'class/class_sequence)
(def-method 'error 'class/class_error)
(def-method 'vdu 'class/class_vdu)
(def-method 'canvas 'class/class_canvas)
(def-method 'points 'class/class_points)
(def-method 'font 'class/class_font)
(def-method 'texture 'class/class_texture)

(defcfun signature (l &rest _)
	(vp-align short_size)
(vp-label l)
	(each (lambda (_)
		(vp-short (list 'sub (label-sym (link-sym (fn-find-link (f-path 'class _)))) '*pc*))) _))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic class construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun gen-class (_1)
	;_1 = class name
	(def-func (f-path 'class _1))
		(vtable-emit _1)
	(def-func-end))

(defcfun gen-create (_1)
	;_1 = class name
	(method-info _1 'create)
	(defq _2 (opt (elem 3 m) '()) _3 (slice 0 (length _2) '(r13 r14 r4 r5 r6 r7 r8 r9 r10 r11 r12)))
	(def-func (sym-cat "class/" _1 "/create"))
		;outputs
		;r0 = 0 if error, else object (ptr)
		;trashes
		;all
		(assign _2 _3)
		(f-call _1 'new '() '(r0))
		(vpif '(r0 != 0))
			;init the object
			(f-call _1 'init `(r0 (@ ,(f-path 'class _1)) ~_3) '(r0 r1))
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
		;r0 = 0 if error, else object (ptr)
		;trashes
		;r1-r3
		(f-jmp 'sys_mem 'alloc `(,(sym-cat _1 "_size")))
	(def-func-end))
