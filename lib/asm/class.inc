;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +vp_regs_calls ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
(defmacro class-sym (_) `(sym (cat "_class_" ,_)))
(defmacro super-sym (_) `(sym (cat "_super_" ,_)))

;;;;;;;;;;;;;;;
; class calling
;;;;;;;;;;;;;;;

(defun method-lookup (_1 _2)
	;_1 = class name
	;_2 = member name
	(defq c (eval (class-sym _1)) _ (rfind _2 (first c)))
	(unless _ (throw "No such method !" (list _1 _2)))
	(elem-get (second c) _))

(defun method-input (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = input paramater index, :nil for entire list
	(if _3
		(elem-get (third (method-lookup _1 _2)) _3)
		(third (method-lookup _1 _2))))

(defun method-output (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = output paramater index, :nil for entire list
	(if _3
		(elem-get (elem-get (method-lookup _1 _2) 3) _3)
		(elem-get (method-lookup _1 _2) 3)))

(defun f-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(first (method-lookup _1 _2)))

(defun s-path (_1 _2)
	;_1 = class name
	;_2 = slot method name
	(first (method-lookup (eval (super-sym _1)) _2)))

(defun f-entry (_1 _2 _3)
	;_1 = class name
	;_2 = slot method name
	;_3 = in parameters
	(assign (third (method-lookup _1 _2)) _3))

(defun f-exit (_1 _2 _3)
	;_1 = class name
	;_2 = slot method name
	;_3 = out parameters
	(assign _3 (elem-get (method-lookup _1 _2) 3)))

(defun l-entry (_1)
	;_1 = in parameters
	(cond
		((not _1))
		((str? _1) (assign (slice +vp_regs_calls 0 (length (split _1 ","))) _1))
		(:t (assign (slice +vp_regs_calls 0 (length _1)) _1))))

(defun l-exit (_1)
	;_1 = out parameters
	(cond
		((not _1))
		((str? _1) (assign _1 (slice +vp_regs_calls 0 (length (split _1 ",")))))
		(:t (assign _1 (slice +vp_regs_calls 0 (length _1))))))

;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

(defcvar (class-sym :nil) '(()()0))

(defun vtable-emit (class)
	(if (eql (defq _ (eval (super-sym class))) :nil)
		(vp-long 0)
		(fn-add-link (f-path _ :vtable)))
	(each (lambda (_)
		(if (or (eql (second _) :virtual) (eql (second _) :final))
			(fn-add-link (first _)))) (second (eval (class-sym class)))))

(defun override ()
	(setq m (cat m))
	(elem-set m 0 mfunc)
	(elem-set (second vtable) s m)
	(if regs_in (elem-set m 2 regs_in))
	(if regs_out (elem-set m 3 regs_out)))

(defmacro def-class (class super &rest lines)
	(defq _ (eval (class-sym super))
		vtable (list (cat (first _)) (cat (second _)) (third _))
		class_sym (class-sym class) super_sym (super-sym class))
	(each (lambda ((_ mname mfunc &optional mtype regs_in regs_out))
		(setd mtype :static)
		(and regs_in (lmatch? regs_in '(abi-args _)) (setq regs_in (eval regs_in)))
		(defq m (if (defq s (rfind mname (first vtable))) (elem-get (second vtable) s)))
		(if m
			(case mtype
				;existing method
				(:static
					(if (eql (second m) :static)
						(override)
						(throw "Virtual method already exists !" mname)))
				(:final
					(case (second m)
						(:virtual
							(override)
							(elem-set m 1 mtype))
						(:final
							(throw "Virtual method is final !" mname))
						(:t (throw "Static method already exists !" mname))))
				(:override
					(case (second m)
						(:virtual
							(override))
						(:final
							(throw "Virtual method is final !" mname))
						(:t (throw "Static method already exists !" mname))))
				(:virtual
					(throw "Method already exists !, use override ?" mname))
				(:t (throw "Must use static, virtual, final or override ! " mtype)))
			(case mtype
				;new method
				(:static
					(push (first vtable) mname)
					(push (second vtable) (list mfunc mtype regs_in regs_out)))
				((:virtual :final)
					(elem-set vtable 2 (setq _ (+ (third vtable) +ptr_size)))
					(push (first vtable) mname)
					(push (second vtable) (list mfunc mtype regs_in regs_out _)))
				(:override
					(throw "Method does not exist !" mname))
				(:t (throw "Must use static, virtual, final or override ! " mtype))))) lines)
	`(def *compile_env* ',super_sym ',super ',class_sym ',vtable))

(defmacro def-method (class member &optional alignment)
	`(def-func (f-path ,class ,member) ,alignment))

;;;;;;;;;;;;;;;;;;
; function helpers
;;;;;;;;;;;;;;;;;;

(defun call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(cond
		((sym? _2)
			(cond
				;f-call
				((eql (second (defq m (method-lookup _1 _2))) :static)
					(assign _3 (third m))
					(fn-call (first m)))
				;v-call
				(:t (defq _ (third m))
					(if (rfind :r14 _)
						(throw "Dispatch register conflicts with arg !" (list :r14 _)))
					(assign _3 _)
					(vp-cpy-ir :r0 obj_vtable :r14)
					(vp-call-i :r14 (elem-get m 4))))
			(if _4 (assign (elem-get m 3) _4)))
		(:t ;l-call
			(cond
				((not _2))
				((str? _2) (assign _2 (slice +vp_regs_calls 0 (length (split _2 ",")))))
				(:t (assign _2 (slice +vp_regs_calls 0 (length _2)))))
			(vp-call _1)
			(cond
				((not _3))
				((str? _3) (assign (slice +vp_regs_calls 0 (length (split _3 ","))) _3))
				(:t (assign (slice +vp_regs_calls 0 (length _3)) _3))))))

(defun jump (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(cond
		((eql (second (defq m (method-lookup _1 _2))) :static)
			(assign _3 (third m))
			(scope-unwind)
			(fn-jump (first m)))
		(:t (defq _ (third m))
			(if (rfind :r14 _)
				(throw "Dispatch register conflicts with arg !" (list :r14 _)))
			(assign _3 _)
			(vp-cpy-ir :r0 obj_vtable :r14)
			(scope-unwind)
			(vp-jmp-i :r14 (elem-get m 4)))))

(defmacro entry (&rest _)
	;either
	;_1 = class name
	;_2 = slot method name
	;_3 = in parameters
	;or
	;_1 = in parameters
	(cat (if (= (length _) 1) '(l-entry) '(f-entry)) _))

(defmacro exit (&rest _)
	;either
	;_1 = class name
	;_2 = slot method name
	;_3 = out parameters
	;or
	;_1 = out parameters
	(cat (if (= (length _) 1) '(l-exit) '(f-exit)) _))

;;;;;;;;;;;;;;;;
; method calling
;;;;;;;;;;;;;;;;

(defun f-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(unless (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is not static ! Use v-call ?" (list _1 _2)))
	(assign _3 (third m))
	(fn-call (first m))
	(if _4 (assign (elem-get m 3) _4)))

(defun f-jmp (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(unless (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is not static ! Use v-jump ?" (list _1 _2)))
	(assign _3 (third m))
	(scope-unwind)
	(fn-jump (first m)))

(defun f-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(unless (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is not static ! Use v-bind ?" (list _1 _2)))
	(fn-bind (first m) _3))

(defun s-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(if (eql (second (defq m (method-lookup (eval (super-sym _1)) _2))) :static)
		(unless (eql _2 :init)
			(throw "Method is static !" (list (eval (super-sym _1)) _2))))
	(assign _3 (third m))
	(unless (eql (first m) 'class/obj/null)
		(fn-call (first m))
		(if _4 (assign (elem-get m 3) _4))))

(defun s-jump (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(if (eql (second (defq m (method-lookup (eval (super-sym _1)) _2))) :static)
		(unless (eql _2 :init)
			(throw "Method is static !" (list (eval (super-sym _1)) _2))))
	(assign _3 (third m))
	(scope-unwind)
	(if (eql (first m) 'class/obj/null)
		(vp-ret)
		(fn-jump (first m))))

(defun s-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(defq m (method-lookup (eval (super-sym _1)) _2))
	(if (eql (second m) :static)
		(throw "Method is static !" (list (eval (super-sym _1)) _2)))
	(fn-bind (first m) _3))

(defun v-call (_1 _2 &optional _3 _4 _5 _6)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = obj reg
	;_6 = dispatch reg
	(setd _5 :r0 _6 :r14)
	(if (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is static ! Use f-call ?" (list _1 _2)))
	(if (rfind _6 (defq _ (third m)))
		(throw "Dispatch register conflicts with arg !" (list _6 _)))
	(assign _3 _)
	(vp-cpy-ir _5 obj_vtable _6)
	(vp-call-i _6 (elem-get m 4))
	(if _4 (assign (elem-get m 3) _4)))

(defun v-jump (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(setd _4 :r14)
	(if (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is static ! Use f-jmp ?" (list _1 _2)))
	(if (rfind _4 (defq _ (third m)))
		(throw "Dispatch register conflicts with arg !" (list _4 _)))
	(assign _3 _)
	(vp-cpy-ir :r0 obj_vtable _4)
	(scope-unwind)
	(vp-jmp-i _4 (elem-get m 4)))

(defun v-bind (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = obj reg
	;_4 = dispatch reg
	(setd _3 :r0 _4 :r14)
	(if (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is static ! Use f-bind ?" (list _1 _2)))
	(vp-cpy-ir _3 obj_vtable _4)
	(vp-cpy-ir _4 (elem-get m 4) _4))

(defun d-call (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	(if (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is static ! Use f-call ?" (list _1 _2)))
	(assign _3 (third m))
	(fn-call (first m))
	(if _4 (assign (elem-get m 3) _4)))

(defun d-jump (_1 _2 &optional _3)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	(if (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is static ! Use f-jmp ?" (list _1 _2)))
	(assign _3 (third m))
	(scope-unwind)
	(fn-jump (first m)))

(defun d-bind (_1 _2 _3)
	;_1 = class name
	;_2 = member name
	;_3 = reg
	(if (eql (second (defq m (method-lookup _1 _2))) :static)
		(throw "Method is static ! Use f-bind ?" (list _1 _2)))
	(fn-bind (first m) _3))

(defun r-call (_1 _2 &optional _3 _4 _5)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	(setd _5 :r14)
	(defq m (method-lookup _1 _2) _ (third m))
	(if (rfind _5 _)
		(throw "Dispatch register conflicts with arg !" (list _5 _)))
	(assign _3 (cat _ (list _5)))
	(vp-call-r _5)
	(if _4 (assign (elem-get m 3) _4)))

(defun r-jump (_1 _2 &optional _3 _4)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = dispatch reg
	(setd _4 :r14)
	(defq m (method-lookup _1 _2) _ (third m))
	(if (rfind _4 _)
		(throw "Dispatch register conflicts with arg !" (list _4 _)))
	(assign _3 (cat _ (list _4)))
	(scope-unwind)
	(vp-jmp-r _4))

(defun l-call (_1 &optional _2 _3)
	;_1 = label
	;_2 = in parameters
	;_3 = out parameters
	(cond
		((not _2))
		((str? _2) (assign _2 (slice +vp_regs_calls 0 (length (split _2 ",")))))
		(:t (assign _2 (slice +vp_regs_calls 0 (length _2)))))
	(vp-call _1)
	(cond
		((not _3))
		((str? _3) (assign (slice +vp_regs_calls 0 (length (split _3 ","))) _3))
		(:t (assign (slice +vp_regs_calls 0 (length _3)) _3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic class construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signature (_)
	(vp-align +short_size)
(vp-label 'sig)
	(each (lambda (_)
		(vp-short `(- ,(label-sym (link-sym (fn-find-link (f-path _ :vtable)))) *pc*))) _))

(defun gen-vtable (_1)
	;_1 = class name
	(def-func (f-path _1 :vtable))
		(vtable-emit _1)
	(def-func-end))

(defun gen-create (_1 &optional _2)
	;_1 = class name
	;_2 = create/init name
	(defq m (method-lookup _1 (if _2 (sym (cat :create_ _2)) :create)) _3 (ifn (third m) '())
		_4 (slice '(:r14 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13) 0 (length _3)))
	(def-func (f-path _1 (if _2 (sym (cat :create_ _2)) :create)))
		;inputs
		;...
		;outputs
		;:r0 = 0 if error, else object (ptr)
		;trashes
		;...
		(assign _3 _4)
		(call 'sys_mem :alloc `(,(sym (cat _1 "_size"))) '(:r0 _))
		(vpif '(:r0 /= 0))
			;init the object
			(call _1 (if _2 (sym (cat :init_ _2)) :init) `(:r0 (@ ,(f-path _1 :vtable)) ~_4) '(:r0 :r1))
			(vpif '(:r1 = 0))
				;error with init
				(call 'sys_mem :free '(:r0))
				(vp-xor-rr :r0 :r0)
			(endif)
		(endif)
		(vp-ret)
	(def-func-end))

(defun gen-type (_1)
	(def-func (f-path _1 :type))
		;_1 = class name
		;inputs
		;:r0 = ... object (ptr)
		;outputs
		;:r0 = ... object (ptr)
		;:r1 = type list object (ptr)
		;trashes
		;:r1-:r5
		(entry _1 :type '(:r0))
		(s-call _1 :type '(:r0) '(:r0 :r1))
		(vp-push :r0 :r1)
		(call 'sym :ref_static_sym `(,(sym (cat "static_sym_" _1))) '(:r1))
		(call 'list :push_back '((:rsp 0) :r1) '(:r1 _ _ _))
		(vp-pop :r0 :r1)
		(exit _1 :type '(:r0 :r1))
		(vp-ret)
	(def-func-end))

;module
(export-symbols
	'(gen-type gen-create gen-vtable def-class def-method
	entry exit call jump s-call d-call s-jump v-call d-jump r-call v-bind
	f-entry f-exit l-entry l-exit f-path f-bind
	method-input method-output method-lookup signature))
(env-pop)
