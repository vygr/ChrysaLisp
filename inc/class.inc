;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

(defcfun class-name (n) (sym (cat "_class_" (str n))))
(defcfun super-name (n) (sym (cat "_super_" (str n))))

(defcvar '*class* nil (class-name 'null) '())

(defcfun vtable-emit (%1 &optional %2)
	;%1 = class name
	;%2 = parent class name
	(if %2 (fn-add-link %2) (vp-long 0))
	(each (lambda (m)
		(if (eql (elem 2 m) 'virtual)
			(fn-add-link (elem 1 m)))) (eval (class-name %1))))

(defcfun def-class (%1 &optional %2)
	;%1 = class name
	;%2 = super class name
	(setq %2 (if %2 %2 'null) *class* %1)
	(defcvar (super-name %1) %2 (class-name %1) (cat (eval (class-name %2)))))

(defcfun def-method (%1 %2 &optional %3 %4 %5)
	;%1 = member name
	;%2 = member function
	;%3 = mode (static/virtual/override)
	;%4 = in regs
	;%5 = out regs
	(setq %3 (if %3 %3 'static) %4 (if %4 %4 '()) %5 (if %5 %5 '()))
	(defq s 0 c (eval (class-name *class*))
		m (some (lambda (m)
			(if (eql (elem 0 m) %1) m
				(progn (setq s (inc s)) nil))) c))
	(defun override ()
		(setq m (cat m))
		(elem-set s c m)
		(elem-set 1 m %2)
		(if (ne (length %4) 0) (elem-set 3 m %4))
		(if (ne (length %5) 0) (elem-set 4 m %5)))
	(if m
		(cond
			;existing method
			((eql %3 'static)
				(if (eql (elem 2 m) 'static)
					(override)
					(error "Virtual method allready exists !" %1)))
			((eql %3 'virtual)
				(error "Method allready exists !, use override ?" %1))
			((eql %3 'override)
				(if (eql (elem 2 m) 'virtual)
					(override)
					(error "Static method allready exists !" %1)))
			(t
				(error "Must use static, virtual or override ! " %3)))
		(cond
			;new method
			((or (eql %3 'static) (eql %3 'virtual))
				(push c (list %1 %2 %3 %4 %5)))
			((eql %3 'override)
				(error "Method does not exist !" %1))
			(t
				(error "Must use static, virtual or override ! " %3)))))

;;;;;;;;;;;;;;;
; class calling
;;;;;;;;;;;;;;;

(defcfun method-lookup (%1 %2)
	;%1 = class name
	;%2 = member name
	;writes to s, v, c in parent enviroment !
	(setq s 0 v ptr_size c (eval (class-name %1)))
	(until (or (eq s (length c)) (eql (elem 0 (elem s c)) %2))
		(if (eql (elem 2 (elem s c)) 'virtual) (setq v (add v ptr_size)))
		(setq s (inc s)))
	(if (eq s (length c))
		(error "No such method !" (list %1 %2))))

(defcfun f-path (%1 %2 &optional %3)
	;%1 = class name
	;%2 = slot method name
	;%3 = macro name
	(setq %3 (if %3 %3 '_function_))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defcvar %3 (elem 1 (elem s (eval (class-name %1))))))

(defcfun s-path (%1 %2 &optional %3)
	;%1 = class name
	;%2 = slot method name
	;%3 = macro name
	(setq %3 (if %3 %3 '_function_) n (eval (super-name %1)))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defcvar %3 (elem 1 (elem s (eval n)))))

;;;;;;;;;;;;;;;;
; method calling
;;;;;;;;;;;;;;;;

(defcfun f-call (%1 %2 &optional %3 %4)
	;%1 = class name
	;%2 = member name
	;%3 = in parameters
	;%4 = out parameters
	(setq %3 (if %3 %3 (list)) %4 (if %4 %4 (list)))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'virtual)
		(error "Method is virtual ! Use v-call ?" (list %1 %2)))
	(assign %3 (elem 3 m))
	(fn-call (elem 1 m))
	(assign (elem 4 m) %4))

(defcfun f-jmp (%1 %2 &optional %3)
	;%1 = class name
	;%2 = member name
	;%3 = in parameters
	(setq %3 (if %3 %3 (list)))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'virtual)
		(error "Method is virtual ! Use v-jmp ?" (list %1 %2)))
	(assign %3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun f-bind (%1 %2 %3)
	;%1 = class name
	;%2 = member name
	;%3 = reg
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'virtual)
		(error "Method is virtual ! Use v-bind ?" (list %1 %2)))
	(fn-bind (elem 1 m) %3))

(defcfun s-call (%1 %2 &optional %3 %4)
	;%1 = class name
	;%2 = member name
	;%3 = in parameters
	;%4 = out parameters
	(setq %3 (if %3 %3 (list)) %4 (if %4 %4 (list)))
	(defq s nil v nil c nil)
	(method-lookup (eval (super-name %1)) %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(unless (eql (elem 0 m) 'init)
			(error "Method is static !" (list (eval (super-name %1)) %2))))
	(assign %3 (elem 3 m))
	(fn-call (elem 1 m))
	(assign (elem 4 m) %4))

(defcfun s-jmp (%1 %2 &optional %3)
	;%1 = class name
	;%2 = member name
	;%3 = in parameters
	(setq %3 (if %3 %3 (list)))
	(defq s nil v nil c nil)
	(method-lookup (eval (super-name %1)) %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(unless (eql (elem 0 m) 'init)
			(error "Method is static !" (list (eval (super-name %1)) %2))))
	(assign %3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun s-bind (%1 %2 %3)
	;%1 = class name
	;%2 = member name
	;%3 = reg
	(defq s nil v nil c nil)
	(method-lookup (eval (super-name %1)) %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(error "Method is static !" (list (eval (super-name %1)) %2)))
	(fn-bind (elem 1 m) %3))

(defcfun v-call (%1 %2 &optional %3 %4 %5)
	;%1 = class name
	;%2 = member name
	;%3 = in parameters
	;%4 = out parameters
	;%5 = dispatch reg
	(setq %3 (if %3 %3 (list)) %4 (if %4 %4 (list)) %5 (if %5 %5 r15))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-call ?" (list %1 %2)))
	(setq s (elem 3 m))
	(if (str? %3)
		(setq %3 (cat "RXX, " %3) s (cat (list %5) s)))
	(assign %3 s)
	(vp-cpy-ir r0 obj_vtable %5)
	(vp-call-i %5 v)
	(assign (elem 4 m) %4))

(defcfun v-jmp (%1 %2 &optional %3 %4)
	;%1 = class name
	;%2 = member name
	;%3 = in parameters
	;%4 = dispatch reg
	(setq %3 (if %3 %3 (list)) %4 (if %4 %4 r15))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-jmp ?" (list %1 %2)))
	(setq s (elem 3 m))
	(if (str? %3)
		(setq %3 (cat "RXX, " %3) s (cat (list %4) s)))
	(assign %3 s)
	(vp-cpy-ir r0 obj_vtable %4)
	(vp-jmp-i %4 v))

(defcfun v-bind (%1 %2 &optional %3)
	;%1 = class name
	;%2 = member name
	;%3 = dispatch reg
	(setq %3 (if %3 %3 r15))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-bind ?" (list %1 %2)))
	(vp-cpy-ir r0 obj_vtable %3)
	(vp-cpy-ir %3 v %3))

(defcfun d-call (%1 %2 &optional %3 %4)
	;%1 = class name
	;%2 = member name
	;%3 = in parameters
	;%4 = out parameters
	(setq %3 (if %3 %3 (list)) %4 (if %4 %4 (list)))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-call ?" (list %1 %2)))
	(assign %3 (elem 3 m))
	(fn-call (elem 1 m))
	(assign (elem 4 m) %4))

(defcfun d-jmp (%1 %2 &optional %3)
	;%1 = class name
	;%2 = member name
	;%3 = in parameters
	(setq %3 (if %3 %3 (list)) %4 (if %4 %4 (list)))
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-jmp ?" (list %1 %2)))
	(assign %3 (elem 3 m))
	(fn-jmp (elem 1 m)))

(defcfun d-bind (%1 %2 %3)
	;%1 = class name
	;%2 = member name
	;%3 = reg
	(defq s nil v nil c nil)
	(method-lookup %1 %2)
	(defq m (elem s c))
	(if (eql (elem 2 m) 'static)
		(error "Method is static ! Use f-bind ?" (list %1 %2)))
	(fn-bind (elem 1 m) %3))

(defcfun l-call (%1 &optional %2 %3 %4 %5)
	;%1 = label
	;%2 = in parameters
	;%3 = in regs
	;%4 = out parameters
	;%5 = out regs
	(setq %2 (if %2 %2 (list)) %3 (if %3 %3 (list)) %4 (if %4 %4 (list)) %5 (if %5 %5 (list)))
	(assign %2 %3)
	(vp-call %1)
	(assign %4 %5))

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
(def-method 'error 'class/class_error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic class construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun class-macro-class (%1)
	;%1 = class name
	(f-path 'class %1)
	(def-func _function_)
		(defq c (eval (super-name %1)))
		(if (eql c 'null)
			(vtable-emit %1)
			(progn
				(f-path 'class c)
				(vtable-emit %1 _function_)))
	(def-func-end))

(defcfun class-macro-create (%1)
	;%1 = class name
	(def-func (sym (cat "class/" (str %1) "/create")))
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3
		(f-call %1 'new '() '(r0))
		(vpif '(r0 != 0))
			;init the object
			(f-path 'class %1)
			(f-call %1 'init `(r0 (@ ,_function_)) '(r1))
			(vpif '(r1 == 0))
				;error with init
				(v-call %1 'delete '(r0) '() r1)
				(vp-xor-rr r0 r0)
			(endif)
		(endif)
		(vp-ret)
	(def-func-end))

(defcfun class-macro-new (%1)
	;%1 = class name
	(def-func (sym (cat "class/" (str %1) "/new")))
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3
		(f-call 'sys_mem 'alloc `(,(sym (cat (str %1) "_size"))) '(r0 _))
		(vpif '(r0 != 0))
			;clear object memory
			(vp-cpy-rr r0 r3)
			(f-call 'sys_mem 'clear `(r0 ,(sym (cat (str %1) "_size"))) '(_))
			(vp-cpy-rr r3 r0)
		(endif)
		(vp-ret)
	(def-func-end))
