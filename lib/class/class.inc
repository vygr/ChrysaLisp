;;;;;;;;;;;;;;;;;;;;;
; classes and methods
;;;;;;;;;;;;;;;;;;;;;

(defmacro .? (this method)
	; (.? this method) -> nil | lambda
	`(def? ,method (def? :vtable ,this)))

(defmacro .super (this method &rest _)
	; (.super this :method [arg ...])
	`((def? ,method ,(eval *super_class*)) ,this ~_))

(defmacro defmethod (n a &rest _)
	; (defmethod name (this [arg ...]) body)
	; (. this :method [arg ...])
	`(def (def? :vtable this) ,n (lambda ,a ~(macroexpand _))))

(defmacro defabstractmethod (n a &rest _)
	; (defabstractmethod (this [arg ...]) body)
	; Declare a method as abstract and will
	; throw an error if invoked. Concrete
	; classes deriving from a class with abstractions
	; should provide the concreate handler
	`(def (def? :vtable this) ,n (lambda ,a (throw "Is abstract method !" ,n))))

(defmacro deffimethod (n _)
	; (deffimethod name ffi)
	`(def (def? :vtable this) ,n (ffi _ ,_ 0)))

(defmacro defclass (n a s &rest _)
	; (defclass name ([arg ...]) (super ...) body)
	(defq
		module  (str *stream_name*)
		predn   (sym (cat n "?"))
		kwn     (sym (cat : n))
		super   (sym (cat "*class_" (if s (elem 0 s)) "*"))
		class   (sym (cat "*class_" n "*")) inits (list) methods (list))
	(def (penv) '*class* n '*super_class* super)
	(each (lambda (form)
		(cond
			((and (list? form) (ends-with "method" (elem 0 form)))
				(push methods form))
			(t  (push inits form)))) _)
	(if s
		`(progn
			(defq ,class (env -1))
			(defun ,predn (_o) (and (env? _o) (def? :vtable _o) (find ,kwn (. _o :type_of))))
			(defun ,n ,a
				(def (defq this ,s) :vtable ,class)
				(unless (def? :vtable ,class)
					(def this :vtable (reduce (lambda (e (key val))
						(def e key val) e) (tolist ,super) ,class))
					(def ,class :vtable ,super :module ,module)
					~methods
					(defmethod :type_of (this)
						(push (.super this :type_of) ,kwn)))
				~inits this))
		`(progn
			(defq ,class (env -1))
			(defun ,predn (_o) (and (env? _o) (def? :vtable _o) (find ,kwn (. _o :type_of))))
			(defun ,n ,a
				(def (defq this (env -1)) :vtable ,class)
				(unless (def? :vtable ,class)
					(def ,class :vtable t :module ,module)
					~methods
					(defmethod :type_of (this)
						(push (type-of this) ,kwn)))
				~inits this))))

(defmacro .-> (res &rest frm)
	; (.-> this form ...)
	(reduce (lambda (res frm)
		(if (list? frm)
			(insert frm 0 (list '. res))
			(list '. res frm))) frm res))

(defmacro raise (&rest fields)
	; (raise field | (var val) ...) -> (defq var (get field this) ...)
	(reduce (# (if (list? %1)
			(cat %0 %1)
			(push %0 (sym (slice 1 -1 %1)) `(get ,%1 this))))
		fields (list defq)))

(defmacro lower (&rest fields)
	; (lower field | (field val) ...) -> (set this field var ...)
	(reduce (# (if (list? %1)
			(cat %0 %1)
			(push %0 %1 (sym (slice 1 -1 %1)))))
		fields (list set 'this)))
