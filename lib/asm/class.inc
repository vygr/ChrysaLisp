;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +vp_regs_calls ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
(defcvar '*vtables* (env 1) '*supers* (env 1))

;;;;;;;;;;;;;;;
; class calling
;;;;;;;;;;;;;;;

(defun method-lookup (%0 %1)
	;%0 = class name
	;%1 = member name
	(or (get %1 (get %0 *vtables*))
		(throw "No such method !" (list %0 %1))))

(defun method-input (%0 %1 &optional %2)
	;%0 = class name
	;%1 = member name
	;%2 = input parameter index, :nil for entire list
	(defq %0 (third (method-lookup %0 %1)))
	(if %2 (elem-get %0 %2) %0))

(defun method-output (%0 %1 &optional %2)
	;%0 = class name
	;%1 = member name
	;%2 = output parameter index, :nil for entire list
	(defq %0 (elem-get (method-lookup %0 %1) 3))
	(if %2 (elem-get %0 %2) %0))

(defun f-path (%0 %1)
	;%0 = class name
	;%1 = slot method name
	(first (method-lookup %0 %1)))

(defun s-path (%0 %1)
	;%0 = class name
	;%1 = slot method name
	(first (method-lookup (get %0 *supers*) %1)))

(defun f-entry (%0 %1 %2)
	;%0 = class name
	;%1 = slot method name
	;%2 = in parameters
	(assign (third (method-lookup %0 %1)) %2))

(defun f-exit (%0 %1 %2)
	;%0 = class name
	;%1 = slot method name
	;%2 = out parameters
	(assign %2 (elem-get (method-lookup %0 %1) 3)))

(defun l-entry (%0)
	;%0 = in parameters
	(cond
		((not %0))
		((str? %0) (assign (slice +vp_regs_calls 0 (length (split %0 ","))) %0))
		((assign (slice +vp_regs_calls 0 (length %0)) %0))))

(defun l-exit (%0)
	;%0 = out parameters
	(cond
		((not %0))
		((str? %0) (assign %0 (slice +vp_regs_calls 0 (length (split %0 ",")))))
		((assign %0 (slice +vp_regs_calls 0 (length %0))))))

;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

(def *vtables* :nil (progn (def (defq e (env 1)) :vslot 0) e))

(defun vtable-emit (class)
	(if (eql (defq _ (get class *supers*)) :nil)
		(vp-long 0) (fn-add-link (f-path _ :vtable)))
	(each (lambda ((k v))
			(and (nql :vslot k)
				(find (second v) '(:virtual :final))
				(fn-add-link (first v))))
		(tolist (get class *vtables*))))

(defun override ()
	(elem-set (setq m (cat m)) 0 mfunc)
	(if regs_in (elem-set m 2 regs_in))
	(if regs_out (elem-set m 3 regs_out))
	(def vtable mname m))

(defmacro def-class (class super &rest lines)
	(defq vtable (env-copy (get super *vtables*) 1))
	(each (lambda ((& mname mfunc &optional mtype regs_in regs_out))
		(setd mtype :static)
		(and regs_in (lmatch? regs_in '(abi-args _)) (setq regs_in (eval regs_in)))
		(if (defq m (get mname vtable))
			;existing method
			(case mtype
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
			;new method
			(case mtype
				(:static
					(def vtable mname (list mfunc mtype regs_in regs_out)))
				((:virtual :final)
					(def vtable :vslot (defq _ (num-intern (+ (get :vslot vtable) +ptr_size)))
						mname (list mfunc mtype regs_in regs_out _)))
				(:override
					(throw "Method does not exist !" mname))
				(:t (throw "Must use static, virtual, final or override ! " mtype)))))
			;auto dec :type method
			(ifn (starts-with "class/" (first (repl-info))) lines
				(push lines `(dec-method :type ,(sym (cat "class/" (rest class) "/type"))
					,(if (eql class ":obj") :virtual :override) (:r0) (:r0 :r1)))))
	`(progn (def *vtables* ',class ',vtable) (def *supers* ',class ',super)))

(defmacro def-method (class member &optional alignment)
	`(def-func (f-path ,class ,member) ,alignment))

;;;;;;;;;;;;;;;;;;
; function helpers
;;;;;;;;;;;;;;;;;;

(defun call (%0 %1 &optional %2 %3)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	(cond
		((sym? %1)
			(cond
				;f-call
				((eql (second (defq m (method-lookup %0 %1))) :static)
					(assign %2 (third m))
					(fn-call (first m)))
				;v-call
				(:t (if (find :r14 (defq _ (third m)))
						(throw "Dispatch register conflicts with arg !" (list :r14 _)))
					(assign %2 _)
					(vp-cpy-ir :r0 +obj_vtable :r14)
					(vp-call-i :r14 (last m))))
			(if %3 (assign (elem-get m 3) %3)))
		(:t ;l-call
			(cond
				((not %1))
				((str? %1) (assign %1 (slice +vp_regs_calls 0 (length (split %1 ",")))))
				((assign %1 (slice +vp_regs_calls 0 (length %1)))))
			(vp-call %0)
			(cond
				((not %2))
				((str? %2) (assign (slice +vp_regs_calls 0 (length (split %2 ","))) %2))
				((assign (slice +vp_regs_calls 0 (length %2)) %2))))))

(defun jump (%0 %1 &optional %2)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	(cond
		((eql (second (defq m (method-lookup %0 %1))) :static)
			(assign %2 (third m))
			(scope-unwind)
			(fn-jump (first m)))
		(:t (if (find :r14 (defq _ (third m)))
				(throw "Dispatch register conflicts with arg !" (list :r14 _)))
			(assign %2 _)
			(vp-cpy-ir :r0 +obj_vtable :r14)
			(scope-unwind)
			(vp-jmp-i :r14 (last m)))))

(defmacro entry (&rest %0)
	;either
	;%0 = class name
	;%1 = slot method name
	;%2 = in parameters
	;or
	;%0 = in parameters
	(cat (if (= (length %0) 1) '(l-entry) '(f-entry)) %0))

(defmacro exit (&rest %0)
	;either
	;%0 = class name
	;%1 = slot method name
	;%2 = out parameters
	;or
	;%0 = out parameters
	(cat (if (= (length %0) 1) '(l-exit) '(f-exit)) %0))

;;;;;;;;;;;;;;;;
; method calling
;;;;;;;;;;;;;;;;

(defun f-call (%0 %1 &optional %2 %3)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	(unless (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is not static ! Use v-call ?" (list %0 %1)))
	(assign %2 (third m))
	(fn-call (first m))
	(if %3 (assign (elem-get m 3) %3)))

(defun f-jmp (%0 %1 &optional %2)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	(unless (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is not static ! Use v-jump ?" (list %0 %1)))
	(assign %2 (third m))
	(scope-unwind)
	(fn-jump (first m)))

(defun f-bind (%0 %1 %2)
	;%0 = class name
	;%1 = member name
	;%2 = reg
	(unless (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is not static ! Use v-bind ?" (list %0 %1)))
	(fn-bind (first m) %2))

(defun s-call (%0 %1 &optional %2 %3)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	(if (eql (second (defq m (method-lookup (get %0 *supers*) %1))) :static)
		(unless (eql %1 :init)
			(throw "Method is static !" (list (get %0 *supers*) %1))))
	(assign %2 (third m))
	(unless (eql (first m) 'class/obj/null)
		(fn-call (first m))
		(if %3 (assign (elem-get m 3) %3))))

(defun s-jump (%0 %1 &optional %2)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	(if (eql (second (defq m (method-lookup (get %0 *supers*) %1))) :static)
		(unless (eql %1 :init)
			(throw "Method is static !" (list (get %0 *supers*) %1))))
	(assign %2 (third m))
	(scope-unwind)
	(if (eql (first m) 'class/obj/null)
		(vp-ret)
		(fn-jump (first m))))

(defun s-bind (%0 %1 %2)
	;%0 = class name
	;%1 = member name
	;%2 = reg
	(defq m (method-lookup (get %0 *supers*) %1))
	(if (eql (second m) :static)
		(throw "Method is static !" (list (get %0 *supers*) %1)))
	(fn-bind (first m) %2))

(defun v-call (%0 %1 &optional %2 %3 %4 %5)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	;%4 = obj reg
	;%5 = dispatch reg
	(if (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is static ! Use f-call ?" (list %0 %1)))
	(if (find (setd %4 :r0 %5 :r14) (defq _ (third m)))
		(throw "Dispatch register conflicts with arg !" (list %5 _)))
	(assign %2 _)
	(vp-cpy-ir %4 +obj_vtable %5)
	(vp-call-i %5 (last m))
	(if %3 (assign (elem-get m 3) %3)))

(defun v-jump (%0 %1 &optional %2 %3 %4)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = obj reg
	;%4 = dispatch reg
	(if (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is static ! Use f-jmp ?" (list %0 %1)))
	(if (find (setd %3 :r0 %4 :r14) (defq _ (third m)))
		(throw "Dispatch register conflicts with arg !" (list %4 _)))
	(assign %2 _)
	(vp-cpy-ir %3 +obj_vtable %4)
	(scope-unwind)
	(vp-jmp-i %4 (last m)))

(defun v-bind (%0 %1 &optional %2 %3)
	;%0 = class name
	;%1 = member name
	;%2 = obj reg
	;%3 = dispatch reg
	(setd %2 :r0 %3 :r14)
	(if (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is static ! Use f-bind ?" (list %0 %1)))
	(vp-cpy-ir %2 +obj_vtable %3)
	(vp-cpy-ir %3 (last m) %3))

(defun d-call (%0 %1 &optional %2 %3)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	(if (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is static ! Use f-call ?" (list %0 %1)))
	(assign %2 (third m))
	(fn-call (first m))
	(if %3 (assign (elem-get m 3) %3)))

(defun d-jump (%0 %1 &optional %2)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	(if (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is static ! Use f-jmp ?" (list %0 %1)))
	(assign %2 (third m))
	(scope-unwind)
	(fn-jump (first m)))

(defun d-bind (%0 %1 %2)
	;%0 = class name
	;%1 = member name
	;%2 = reg
	(if (eql (second (defq m (method-lookup %0 %1))) :static)
		(throw "Method is static ! Use f-bind ?" (list %0 %1)))
	(fn-bind (first m) %2))

(defun r-call (%0 %1 &optional %2 %3 %4)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	;%4 = dispatch reg
	(defq m (method-lookup %0 %1) _ (third m))
	(if (find (setd %4 :r14) _)
		(throw "Dispatch register conflicts with arg !" (list %4 _)))
	(assign %2 (cat _ (list %4)))
	(vp-call-r %4)
	(if %3 (assign (elem-get m 3) %3)))

(defun r-jump (%0 %1 &optional %2 %3)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = dispatch reg
	(defq m (method-lookup %0 %1) _ (third m))
	(if (find (setd %3 :r14) _)
		(throw "Dispatch register conflicts with arg !" (list %3 _)))
	(assign %2 (cat _ (list %3)))
	(scope-unwind)
	(vp-jmp-r %3))

(defun l-call (%0 &optional %1 %2)
	;%0 = label
	;%1 = in parameters
	;%2 = out parameters
	(cond
		((not %1))
		((str? %1) (assign %1 (slice +vp_regs_calls 0 (length (split %1 ",")))))
		((assign %1 (slice +vp_regs_calls 0 (length %1)))))
	(vp-call %0)
	(cond
		((not %2))
		((str? %2) (assign (slice +vp_regs_calls 0 (length (split %2 ","))) %2))
		((assign (slice +vp_regs_calls 0 (length %2)) %2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic class construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signature (%0)
	(vp-align +short_size)
(vp-label 'sig)
	(each (lambda (%0)
		(vp-short `(- ,(label-sym (link-sym (fn-find-link (f-path %0 :vtable)))) *pc*))) %0))

(defun gen-vtable (%0)
	;%0 = class name
	(def-func (f-path %0 :vtable))
		(vtable-emit %0)
	(def-func-end))

(defun gen-create (%0 &optional %1)
	;%0 = class name
	;%1 = create/init name
	(defq m (method-lookup %0 (if %1 (sym (cat :create_ %1)) :create)) %2 (ifn (third m) '())
		%3 (slice '(:r14 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13) 0 (length %2)))
	(def-func (f-path %0 (if %1 (sym (cat :create_ %1)) :create)))
		;inputs
		;...
		;outputs
		;:r0 = 0 if error, else object (ptr)
		;trashes
		;...
		(assign %2 %3)
		(call :sys_mem :alloc `(,(sym (cat "+" (rest %0) "_size"))) '(:r0 _))
		(vpif '(:r0 /= 0))
			;init the object
			(call %0 (if %1 (sym (cat :init_ %1)) :init) `(:r0 (@ ,(f-path %0 :vtable)) ~%3) '(:r0 :r1))
			(vpif '(:r1 = 0))
				;error with init
				(call :sys_mem :free '(:r0))
				(vp-xor-rr :r0 :r0)
			(endif)
		(endif)
		(vp-ret)
	(def-func-end))

(defun gen-type (%0)
	(def-func (f-path %0 :type))
		;%0 = class name
		;inputs
		;:r0 = ... object (ptr)
		;outputs
		;:r0 = ... object (ptr)
		;:r1 = type list object (ptr)
		;trashes
		;:r1-:r5
		(entry %0 :type '(:r0))
		(s-call %0 :type '(:r0) '(:r0 :r1))
		(vp-push :r0 :r1)
		(call :sym :ref_static_sym `(,(sym (cat "static_sym_" (rest %0)))) '(:r1))
		(call :list :push_back '((:rsp 0) :r1))
		(vp-pop :r0 :r1)
		(exit %0 :type '(:r0 :r1))
		(vp-ret)
	(def-func-end))

;module
(export-symbols
	'(gen-type gen-create gen-create1 gen-vtable def-class def-method
	entry exit call jump s-call d-call s-jump
	v-call v-jump d-jump r-call v-bind
	f-entry f-exit l-entry l-exit f-path f-bind
	method-input method-output method-lookup signature))
(env-pop)
