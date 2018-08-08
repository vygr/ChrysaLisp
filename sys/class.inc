;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

(defmacro class-sym (_)
	`(sym-cat "_class_" ,_))

(defmacro super-sym (_)
	`(sym-cat "_super_" ,_))

(defcvar '*class* nil (class-sym 'null) '(()()) '*args* (list))
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
		(fn-add-link (f-path s 'vtable)))
	(each (lambda (_)
		(if (or (eql (elem 1 _) 'virtual) (eql (elem 1 _) 'final))
			(fn-add-link (elem 0 _)))) (elem 1 (eval (class-sym _1)))))

(defcfun def-class (_1 &optional _2)
	;_1 = class name
	;_2 = super class name
	(setd _2 'null)
	(defq _ (eval (class-sym _2)))
	(setq *class* (list (cat (elem 0 _)) (cat (elem 1 _))))
	(defcvar (super-sym _1) _2 (class-sym _1) *class*))

(defcfun dec-method (_1 _2 &optional _3 _4 _5)
	;_1 = member name
	;_2 = member function
	;_3 = mode (static/virtual/final/override)
	;_4 = in regs
	;_5 = out regs
	(setd _3 'static)
	(defq m (if (defq s (find _1 (elem 0 *class*))) (elem s (elem 1 *class*))))
	(defun override ()
		(setq m (cat m))
		(elem-set 0 m _2)
		(elem-set s (elem 1 *class*) m)
		(if _4 (elem-set 2 m (intern-args _4)))
		(if _5 (elem-set 3 m (intern-args _5))))
	(if m
		(cond
			;existing method
			((eql _3 'static)
				(if (eql (elem 1 m) 'static)
					(override)
					(throw "Virtual method already exists !" _1)))
			((eql _3 'virtual)
				(throw "Method already exists !, use override ?" _1))
			((eql _3 'override)
				(cond
					((eql (elem 1 m) 'virtual)
						(override))
					((eql (elem 1 m) 'final)
						(throw "Virtual method is final !" _1))
					(t (throw "Static method already exists !" _1))))
			((eql _3 'final)
				(cond
					((eql (elem 1 m) 'virtual)
						(override)
						(elem-set 1 m _3))
					((eql (elem 1 m) 'final)
						(throw "Virtual method is final !" _1))
					(t (throw "Static method already exists !" _1))))
			(t (throw "Must use static, virtual, final or override ! " _3)))
		(cond
			;new method
			((or (eql _3 'static) (eql _3 'virtual) (eql _3 'final))
				(push (elem 0 *class*) _1)
				(push (elem 1 *class*) (list _2 _3 (intern-args _4) (intern-args _5))))
			((eql _3 'override)
				(throw "Method does not exist !" _1))
			(t (throw "Must use static, virtual, final or override ! " _3)))))

(defcfun def-method (_1 _2)
	;_1 = class name
	;_2 = member name
	(def-func (f-path _1 _2)))

;;;;;;;;;;;;;;;
; class calling
;;;;;;;;;;;;;;;

(defmacro method-info (_1 _2)
	`(progn
		(defq s nil v ptr_size m nil)
		(method-lookup ,_1 ,_2)))

(defcfun method-lookup (_1 _2)
	;_1 = class name
	;_2 = member name
	;writes to s, v, m in parent environment !
	(defq c (eval (class-sym _1)))
	(setq s (find _2 (elem 0 c)))
	(unless s (throw "No such method !" (list _1 _2)))
	(setq m (elem s (elem 1 c)))
	(each! nil s nil (lambda (_)
		(if (or (eql (elem 1 _) 'virtual) (eql (elem 1 _) 'final))
			(setq v (add v ptr_size)))) (list (elem 1 c))))

(defcfun f-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(method-info _1 _2)
	(elem 0 m))

(defcfun s-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(method-info (eval (super-sym _1)) _2)
	(elem 0 m))

(defcfun f-entry (_1 _2 _3)
	;_1 = class name
	;_2 = slot method name
	;_3 = in parameters
	(method-info _1 _2)
	(assign (elem 2 m) _3))

(defcfun f-exit (_1 _2 _3)
	;_1 = class name
	;_2 = slot method name
	;_3 = out parameters
	(method-info _1 _2)
	(assign _3 (elem 3 m)))

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
	(unless (eql (elem 1 m) 'static)
		(throw "Method is not static ! Use v-call ?" (list _1 _2)))
	(assign _3 (elem 2 m))
	(fn-call (elem 0 m))
	(if _4 (assign (elem 3 m) _4)))

(defcfun f-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info _1 _2)
	(unless (eql (elem 1 m) 'static)
		(throw "Method is not static ! Use v-jmp ?" (list _1 _2)))
	(assign _3 (elem 2 m))
	(fn-jmp (elem 0 m)))

(defcfun f-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info _1 _2)
	(unless (eql (elem 1 m) 'static)
		(throw "Method is not static ! Use v-bind ?" (list _1 _2)))
	(fn-bind (elem 0 m) _3))

(defcfun s-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(method-info (eval (super-sym _1)) _2)
	(if (eql (elem 1 m) 'static)
		(unless (eql _2 'init)
			(throw "Method is static !" (list (eval (super-sym _1)) _2))))
	(assign _3 (elem 2 m))
	(fn-call (elem 0 m))
	(if _4 (assign (elem 3 m) _4)))

(defcfun s-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info (eval (super-sym _1)) _2)
	(if (eql (elem 1 m) 'static)
		(unless (eql _2 'init)
			(throw "Method is static !" (list (eval (super-sym _1)) _2))))
	(assign _3 (elem 2 m))
	(fn-jmp (elem 0 m)))

(defcfun s-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info (eval (super-sym _1)) _2)
	(if (eql (elem 1 m) 'static)
		(throw "Method is static !" (list (eval (super-sym _1)) _2)))
	(fn-bind (elem 0 m) _3))

(defcfun v-call (_1 _2 &optional _3 _4 _5)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	(setd _5 r14)
	(method-info _1 _2)
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-call ?" (list _1 _2)))
	(setq s (elem 2 m))
	(if (find _5 s)
		(throw "Dispatch register conflicts with arg !" (list _5 s)))
	(assign _3 s)
	(vp-cpy-ir r0 obj_vtable _5)
	(vp-call-i _5 v)
	(if _4 (assign (elem 3 m) _4)))

(defcfun v-jmp (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(setd _4 r14)
	(method-info _1 _2)
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-jmp ?" (list _1 _2)))
	(setq s (elem 2 m))
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
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-bind ?" (list _1 _2)))
	(vp-cpy-ir r0 obj_vtable _3)
	(vp-cpy-ir _3 v _3))

(defcfun d-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(method-info _1 _2)
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-call ?" (list _1 _2)))
	(assign _3 (elem 2 m))
	(fn-call (elem 0 m))
	(if _4 (assign (elem 3 m) _4)))

(defcfun d-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(method-info _1 _2)
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-jmp ?" (list _1 _2)))
	(assign _3 (elem 2 m))
	(fn-jmp (elem 0 m)))

(defcfun d-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(method-info _1 _2)
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-bind ?" (list _1 _2)))
	(fn-bind (elem 0 m) _3))

(defcfun r-call (_1 _2 &optional _3 _4 _5)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	(setd _5 r14)
	(method-info _1 _2)
	(setq s (elem 2 m))
	(if (find _5 s)
		(throw "Dispatch register conflicts with arg !" (list _5 s)))
	(assign _3 (cat s (list _5)))
	(vp-call-r _5)
	(if _4 (assign (elem 3 m) _4)))

(defcfun r-jmp (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(setd _4 r14)
	(method-info _1 _2)
	(setq s (elem 2 m))
	(if (find _4 s)
		(throw "Dispatch register conflicts with arg !" (list _4 s)))
	(assign _3 (cat s (list _4)))
	(vp-jmp-r _4))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic class construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun signature (l &rest _)
	(vp-align short_size)
(vp-label l)
	(each (lambda (_)
		(vp-short (list 'sub (label-sym (link-sym (fn-find-link (f-path _ 'vtable)))) '*pc*))) _))

(defcfun gen-class (_1)
	;_1 = class name
	(def-func (f-path _1 'vtable))
		(vtable-emit _1)
	(def-func-end))

(defcfun gen-create (_1)
	;_1 = class name
	(method-info _1 'create)
	(defq _2 (opt (elem 2 m) '()) _3 (slice 0 (length _2) '(r13 r14 r4 r5 r6 r7 r8 r9 r10 r11 r12)))
	(def-func (f-path _1 'create))
		;outputs
		;r0 = 0 if error, else object (ptr)
		;trashes
		;all
		(assign _2 _3)
		(f-call _1 'new nil '(r0))
		(vpif '(r0 != 0))
			;init the object
			(f-call _1 'init `(r0 (@ ,(f-path _1 'vtable)) ~_3) '(r0 r1))
			(vpif '(r1 == 0))
				;error with init
				(v-call _1 'delete '(r0) nil r1)
				(vp-xor-rr r0 r0)
			(endif)
		(endif)
		(vp-ret)
	(def-func-end))

(defcfun gen-new (_1)
	;_1 = class name
	(def-func (f-path _1 'new))
		;outputs
		;r0 = 0 if error, else object (ptr)
		;trashes
		;r1-r3
		(f-jmp 'sys_mem 'alloc `(,(sym-cat _1 "_size")))
	(def-func-end))
