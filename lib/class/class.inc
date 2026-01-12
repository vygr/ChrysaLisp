;;;;;;;;;;;;;;;;;;;;;
; classes and methods
;;;;;;;;;;;;;;;;;;;;;

(defmacro .? (this method)
	; (.? this method) -> :nil | lambda
	(static-qq (def? ,method (def? :vtable ,this))))

(defmacro .super (this method &rest _)
	; (.super this :method [arg ...])
	(static-qq ((def? ,method ,(eval *super_class*)) ,this ~_)))

(defmacro defmethod (n a &rest _)
	; (defmethod name ([arg ...]) body)
	; (. this :method [arg ...])
	`(def *vtable* ,n (lambda (this ~a) ~_)))

(defmacro defabstractmethod (n a &rest _)
	; (defabstractmethod ([arg ...]) body)
	; Declare a method as abstract and will
	; throw an error if invoked. Concrete
	; classes deriving from a class with abstractions
	; should provide the concreate handler
	`(def *vtable* ,n (lambda (this ~a)
		(throw "Is abstract method !" ,n))))

(defmacro deffimethod (n _)
	; (deffimethod name ffi)
	(static-qq (def *vtable* ,n (ffi ,_))))

(defmacro defgetmethod (field)
	; (defgetmethod :field)
	(static-qq (defmethod ,(sym (cat :get_ (rest field))) ()
		(def? ,field this))))

(defmacro defsetmethod (field)
	; (defsetmethod :field)
	(static-qq (defmethod ,(sym (cat :set_ (rest field))) (val)
		(def this ,field val) this)))

(defmacro defproxymethod (n a field)
	; (defproxymethod name field [arg ...])
	(static-qq (defmethod ,n (~a)
		(. (get ,field this) ,n ~a) this)))

(defmacro defclass (n a s &rest _)
	; (defclass Name ([arg ...]) (super ...)|:nil body)
	(defq predn (sym (cat n "?")) kwn (sym (cat : n))
		super (sym (cat "*class_" (if s (first s)) "*"))
		class (sym (cat "*class_" n "*"))
		inits (list) methods (list))
	(def (penv) '*class* n '*super_class* super)
	(each (lambda (form)
		(cond
			((and (list? form) (ends-with "method" (first form)))
				(push methods form))
			(:t (push inits form)))) _)
	(if s
		`(progn
			(def (defq *vtable* (defq ,class (env-copy ,super 1))) :vtable ,super)
			(defmethod :type_of () (push (.super this :type_of) ,kwn))
			~methods
			(defun ,predn (this)
				(and (env? this) (def? :vtable this) (find ,kwn (. this :type_of))))
			(defun ,n ,a
				(def (defq this ,s) :vtable ,class)
				~inits this))
		`(progn
			(def (defq *vtable* (defq ,class (env 1))) :vtable :nil)
			(defmethod :type_of () (push (type-of this) ,kwn))
			~methods
			(defun ,predn (this)
				(and (env? this) (def? :vtable this) (find ,kwn (. this :type_of))))
			(defun ,n ,a
				(def (defq this (env 1)) :vtable ,class)
				~inits this))))

(defmacro .-> (res &rest frm)
	; (.-> this form ...)
	(reduce (lambda (res frm)
		(if (list? frm)
			(insert frm 0 (list (const .) res))
			(list (const .) res frm))) frm res))

(defmacro raise (&rest fields)
	; (raise field | (sym val) ...) -> (defq sym (get field this) ...)
	(reduce (# (if (list? %1)
			(cat %0 %1)
			(push %0 (sym (rest %1)) (static-qq (get ,%%1 this)))))
		fields (list (const defq))))

(defmacro lower (&rest fields)
	; (lower field | (field sym) ...) -> (set this field sym ...)
	(reduce (# (if (list? %1)
			(cat %0 %1)
			(push %0 %1 (sym (rest %1)))))
		fields (list (const def) 'this)))
