;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

(include 'sys/code.inc)

(defmacro class-sym (_)
	`(sym (cat "_class_" ,_)))

(defmacro super-sym (_)
	`(sym (cat "_super_" ,_)))

(defcvar '*class* nil (class-sym 'null) '(()()0) '*args* (list))
(each (lambda (_) (push *args* (list))) (range 0 16))

(defcfun intern-args (l)
	(when l
		(defq _ (some! nil nil t (lambda (_)
			(and (match? _ l) _)) (list (defq b (elem (length l) *args*)))))
		(if _ _ (progn (push b l) l))))

(defcfun vtable-emit (_1)
	;_1 = class name
	(if (eql (defq _ (eval (super-sym _1))) 'null)
		(vp-long 0)
		(fn-add-link (f-path _ 'vtable)))
	(each (lambda (_)
		(if (or (eql (elem 1 _) 'virtual) (eql (elem 1 _) 'final))
			(fn-add-link (elem 0 _)))) (elem 1 (eval (class-sym _1)))))

(defcfun def-class (_1 &optional _2)
	;_1 = class name
	;_2 = super class name
	(setd _2 'null)
	(defq _ (eval (class-sym _2)))
	(setq *class* (list (cat (elem 0 _)) (cat (elem 1 _)) (elem 2 _)))
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
		(case _3
			;existing method
			(static
				(if (eql (elem 1 m) 'static)
					(override)
					(throw "Virtual method already exists !" _1)))
			(final
				(case (elem 1 m)
					(virtual
						(override)
						(elem-set 1 m _3))
					(final
						(throw "Virtual method is final !" _1))
					(otherwise (throw "Static method already exists !" _1))))
			(override
				(case (elem 1 m)
					(virtual
						(override))
					(final
						(throw "Virtual method is final !" _1))
					(otherwise (throw "Static method already exists !" _1))))
			(virtual
				(throw "Method already exists !, use override ?" _1))
			(otherwise (throw "Must use static, virtual, final or override ! " _3)))
		(case _3
			;new method
			(static
				(push (elem 0 *class*) _1)
				(push (elem 1 *class*) (list _2 _3 (intern-args _4) (intern-args _5))))
			((virtual final)
				(push (elem 0 *class*) _1)
				(push (elem 1 *class*) (list _2 _3 (intern-args _4) (intern-args _5)
					(elem-set 2 *class* (add (elem 2 *class*) ptr_size)))))
			(override
				(throw "Method does not exist !" _1))
			(otherwise (throw "Must use static, virtual, final or override ! " _3)))))

(defcfun def-method (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = function stack alignment
	(def-func (f-path _1 _2) _3))

;;;;;;;;;;;;;;;;;;
; function helpers
;;;;;;;;;;;;;;;;;;

(defcfun call (_1 _2 &optional _3 _4 _5)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	(cond
		((sym? _2)
			(defq m (method-lookup _1 _2))
			(cond
				;f-call
				((eql (elem 1 m) 'static)
					(assign _3 (elem 2 m))
					(fn-call (elem 0 m))
					(if _4 (assign (elem 3 m) _4)))
				;v-call
				(t
					(setd _5 r14)
					(defq _ (elem 2 m))
					(if (find _5 _)
						(throw "Dispatch register conflicts with arg !" (list _5 _)))
					(assign _3 _)
					(vp-cpy-ir r0 obj_vtable _5)
					(vp-call-i _5 (elem 4 m))
					(if _4 (assign (elem 3 m) _4)))))
		(t
			;l-call
			(defq _ '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
			(cond
				((not _2))
				((str? _2) (assign _2 (slice 0 (length (split _2 ",")) _)))
				(t (assign _2 (slice 0 (length _2) _))))
			(vp-call _1)
			(cond
				((not _3))
				((str? _3) (assign (slice 0 (length (split _3 ",")) _) _3))
				(t (assign (slice 0 (length _3) _) _3))))))

(defcfun jump (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(defq m (method-lookup _1 _2))
	(cond
		((eql (elem 1 m) 'static)
			(assign _3 (elem 2 m))
			(unwind)
			(fn-jump (elem 0 m)))
		(t
			(setd _4 r14)
			(defq _ (elem 2 m))
			(if (find _4 _)
				(throw "Dispatch register conflicts with arg !" (list _4 _)))
			(assign _3 _)
			(vp-cpy-ir r0 obj_vtable _4)
			(unwind)
			(vp-jmp-i _4 (elem 4 m)))))

(defcmacro entry (&rest _)
	;either
	;_1 = class name
	;_2 = slot method name
	;_3 = in parameters
	;or
	;_1 = in parameters
	(if (eq (length _) 1)
		(cat (list 'l-entry) _)
		(cat (list 'f-entry) _)))

(defcmacro exit (&rest _)
	;either
	;_1 = class name
	;_2 = slot method name
	;_3 = out parameters
	;or
	;_1 = out parameters
	(if (eq (length _) 1)
		(cat (list 'l-exit) _)
		(cat (list 'f-exit) _)))

;;;;;;;;;;;;;;;
; class calling
;;;;;;;;;;;;;;;

(defcfun method-lookup (_1 _2)
	;_1 = class name
	;_2 = member name
	(defq c (eval (class-sym _1)) _ (find _2 (elem 0 c)))
	(unless _ (throw "No such method !" (list _1 _2)))
	(elem _ (elem 1 c)))

(defcfun method-input (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = input paramater index, nil for entire list
	(if _3
		(elem _3 (elem 2 (method-lookup _1 _2)))
		(elem 2 (method-lookup _1 _2))))

(defcfun method-output (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = output paramater index, nil for entire list
	(if _3
		(elem _3 (elem 3 (method-lookup _1 _2)))
		(elem 3 (method-lookup _1 _2))))

(defcfun f-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(elem 0 (method-lookup _1 _2)))

(defcfun s-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(elem 0 (method-lookup (eval (super-sym _1)) _2)))

(defcfun f-entry (_1 _2 _3)
	;_1 = class name
	;_2 = slot method name
	;_3 = in parameters
	(assign (elem 2 (method-lookup _1 _2)) _3))

(defcfun f-exit (_1 _2 _3)
	;_1 = class name
	;_2 = slot method name
	;_3 = out parameters
	(assign _3 (elem 3 (method-lookup _1 _2))))

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
	(defq m (method-lookup _1 _2))
	(unless (eql (elem 1 m) 'static)
		(throw "Method is not static ! Use v-call ?" (list _1 _2)))
	(assign _3 (elem 2 m))
	(fn-call (elem 0 m))
	(if _4 (assign (elem 3 m) _4)))

(defcfun f-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(defq m (method-lookup _1 _2))
	(unless (eql (elem 1 m) 'static)
		(throw "Method is not static ! Use v-jump ?" (list _1 _2)))
	(assign _3 (elem 2 m))
	(unwind)
	(fn-jump (elem 0 m)))

(defcfun f-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(defq m (method-lookup _1 _2))
	(unless (eql (elem 1 m) 'static)
		(throw "Method is not static ! Use v-bind ?" (list _1 _2)))
	(fn-bind (elem 0 m) _3))

(defcfun s-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(defq m (method-lookup (eval (super-sym _1)) _2))
	(if (eql (elem 1 m) 'static)
		(unless (eql _2 'init)
			(throw "Method is static !" (list (eval (super-sym _1)) _2))))
	(assign _3 (elem 2 m))
	(unless (eql (elem 0 m) 'class/obj/null)
		(fn-call (elem 0 m))
		(if _4 (assign (elem 3 m) _4))))

(defcfun s-jump (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(defq m (method-lookup (eval (super-sym _1)) _2))
	(if (eql (elem 1 m) 'static)
		(unless (eql _2 'init)
			(throw "Method is static !" (list (eval (super-sym _1)) _2))))
	(assign _3 (elem 2 m))
	(unwind)
	(if (eql (elem 0 m) 'class/obj/null)
		(vp-ret)
		(fn-jump (elem 0 m))))

(defcfun s-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(defq m (method-lookup (eval (super-sym _1)) _2))
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
	(defq m (method-lookup _1 _2))
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-call ?" (list _1 _2)))
	(defq _ (elem 2 m))
	(if (find _5 _)
		(throw "Dispatch register conflicts with arg !" (list _5 _)))
	(assign _3 _)
	(vp-cpy-ir r0 obj_vtable _5)
	(vp-call-i _5 (elem 4 m))
	(if _4 (assign (elem 3 m) _4)))

(defcfun v-jump (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(setd _4 r14)
	(defq m (method-lookup _1 _2))
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-jmp ?" (list _1 _2)))
	(defq _ (elem 2 m))
	(if (find _4 _)
		(throw "Dispatch register conflicts with arg !" (list _4 _)))
	(assign _3 _)
	(vp-cpy-ir r0 obj_vtable _4)
	(unwind)
	(vp-jmp-i _4 (elem 4 m)))

(defcfun v-bind (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = dispatch reg
	(setd _3 r14)
	(defq m (method-lookup _1 _2))
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-bind ?" (list _1 _2)))
	(vp-cpy-ir r0 obj_vtable _3)
	(vp-cpy-ir _3 (elem 4 m) _3))

(defcfun d-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(defq m (method-lookup _1 _2))
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-call ?" (list _1 _2)))
	(assign _3 (elem 2 m))
	(fn-call (elem 0 m))
	(if _4 (assign (elem 3 m) _4)))

(defcfun d-jump (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(defq m (method-lookup _1 _2))
	(if (eql (elem 1 m) 'static)
		(throw "Method is static ! Use f-jmp ?" (list _1 _2)))
	(assign _3 (elem 2 m))
	(unwind)
	(fn-jump (elem 0 m)))

(defcfun d-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(defq m (method-lookup _1 _2))
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
	(defq m (method-lookup _1 _2) _ (elem 2 m))
	(if (find _5 _)
		(throw "Dispatch register conflicts with arg !" (list _5 _)))
	(assign _3 (cat _ (list _5)))
	(vp-call-r _5)
	(if _4 (assign (elem 3 m) _4)))

(defcfun r-jump (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(setd _4 r14)
	(defq m (method-lookup _1 _2) _ (elem 2 m))
	(if (find _4 _)
		(throw "Dispatch register conflicts with arg !" (list _4 _)))
	(assign _3 (cat _ (list _4)))
	(unwind)
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

(defcfun gen-vtable (_1)
	;_1 = class name
	(def-func (f-path _1 'vtable))
		(vtable-emit _1)
	(def-func-end))

(defcfun gen-create (_1 &optional _2)
	;_1 = class name
	;_2 = create/init name
	(defq m (method-lookup _1 (if _2 (sym (cat "create_" _2)) 'create)) _3 (opt (elem 2 m) '())
		_4 (slice 0 (length _3) '(r14 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13)))
	(def-func (f-path _1 (if _2 (sym (cat "create_" _2)) 'create)))
		;inputs
		;depends on init method
		;outputs
		;r0 = 0 if error, else object (ptr)
		;trashes
		;depends on init method
		(assign _3 _4)
		(call 'sys_mem 'alloc `(,(sym (cat _1 "_size"))) '(r0 _))
		(vpif '(r0 != 0))
			;init the object
			(call _1 (if _2 (sym (cat "init_" _2)) 'init) `(r0 (@ ,(f-path _1 'vtable)) ~_4) '(r0 r1))
			(vpif '(r1 == 0))
				;error with init
				(call 'sys_mem 'free '(r0))
				(vp-xor-rr r0 r0)
			(endif)
		(endif)
		(vp-ret)
	(def-func-end))
