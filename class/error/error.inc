(import 'class/ref/ref.inc)

(def-class 'error 'ref)
(def-method 'create 'class/error/create 'static '(r0 r1 r2 r3) '(r0))
(def-method 'new 'class/error/new 'static nil '(r0))
(def-method 'init 'class/error/init 'static '(r0 r1 r2 r3 r5 r6) '(r1))
(def-method 'get_description 'class/error/get_description 'static '(r0) '(r1))
(def-method 'get_object 'class/error/get_object 'static '(r0) '(r1))
(def-method 'get_file 'class/error/get_file 'static '(r0) '(r1))
(def-method 'get_line 'class/error/get_line 'static '(r0) '(r1))

(def-method 'deinit 'class/error/deinit 'final)

(def-struct 'error 'ref)
	(ptr 'error_description)
	(ptr 'error_object)
	(ptr 'error_file)
	(ulong 'error_line)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/error/get_description ()
	;inputs
	;r0 = error object
	;outputs
	;r0 = error object
	;r1 = string object
	(vp-cpy-ir r0 error_description r1))

(defcfun class/error/get_file ()
	;inputs
	;r0 = error object
	;outputs
	;r0 = error object
	;r1 = string object
	(vp-cpy-ir r0 error_file r1))

(defcfun class/error/get_object ()
	;inputs
	;r0 = error object
	;outputs
	;r0 = error object
	;r1 = error payload object
	(vp-cpy-ir r0 error_object r1))

(defcfun class/error/get_line ()
	;inputs
	;r0 = error object
	;outputs
	;r0 = error object
	;r1 = line number
	(vp-cpy-ir r0 error_line r1))
