(import 'class/sequence/sequence.inc)

(def-class 'string 'sequence)
(def-method 'create_raw 'class/string/create_raw 'static '(r0) '(r0))
(def-method 'create_from_buffer 'class/string/create_from_buffer 'static '(r0 r1) '(r0))
(def-method 'create_from_cstr 'class/string/create_from_cstr 'static '(r0) '(r0))
(def-method 'create_from_file 'class/string/create_from_file 'static '(r0) '(r0))
(def-method 'create_from_long 'class/string/create_from_long 'static '(r0 r1) '(r0))
(def-method 'append 'class/string/append 'static '(r0 r1) '(r0))
(def-method 'cat 'class/string/cat 'static '(r0) '(r0))
(def-method 'new 'class/string/new 'static '(r0) '(r0))
(def-method 'init 'class/string/init 'static '(r0 r1 r2 r3) '(r1))
(def-method 'init1 'class/string/init1 'static '(r0 r1 r2 r3) '(r1))
(def-method 'init2 'class/string/init2 'static '(r0 r1 r2 r3) '(r1))
(def-method 'init3 'class/string/init3 'static '(r0 r1 r2) '(r1))
(def-method 'split 'class/string/split 'static '(r0 r1) '(r1))
(def-method 'compare 'class/string/compare 'static '(r0 r1) '(r1))
(def-method 'same 'class/string/same 'static '(r0 r1) '(r1))
(def-method 'find 'class/string/find 'static '(r0 r1) '(r1))

(def-method 'hash 'class/string/hash 'override)
(def-method 'get_length 'class/string/get_length 'override)
(def-method 'ref_element 'class/string/ref_element 'override)
(def-method 'slice 'class/string/slice 'override)

(def-struct 'string 'sequence)
	(ulong 'string_length)
	(ulong 'string_hashcode)
	(offset 'string_data)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/string/new ()
	;inputs
	;r0 = object size
	;outputs
	;r0 = 0 if error, else object
	;trashes
	;r1-r3
	(f-call 'sys_mem 'alloc '(r0) '(r0 _)))

(defcfun class/string/get_length ()
	;inputs
	;r0 = string object
	;outputs
	;r0 = string object
	;r1 = string length
	(vp-cpy-ir r0 string_length r1))
