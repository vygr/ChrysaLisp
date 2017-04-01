(import 'class/sliceable/sliceable.inc)

(def-class 'string 'sliceable)
(def-method 'create_raw 'class/string/create_raw 'static '(r0) '(r0))
(def-method 'create_from_buffer 'class/string/create_from_buffer 'static '(r0 r1) '(r0))
(def-method 'create_from_cstr 'class/string/create_from_cstr 'static '(r0) '(r0))
(def-method 'create_from_file 'class/string/create_from_file 'static '(r0) '(r0))
(def-method 'create_from_long 'class/string/create_from_long 'static '(r0 r1) '(r0))
(def-method 'append 'class/string/append 'static '(r0 r1) '(r0))
(def-method 'cat 'class/string/cat 'static '(r0) '(r0))
(def-method 'new 'class/string/new 'static '(r0) '(r0))
(def-method 'init 'class/string/init 'static '(r0 r1 r2 r3))
(def-method 'init1 'class/string/init1 'static '(r0 r1 r2 r3) '(r0 r1))
(def-method 'init2 'class/string/init2 'static '(r0 r1 r2 r3) '(r0 r1))
(def-method 'init3 'class/string/init3 'static '(r0 r1 r2) '(r0 r1))
(def-method 'split 'class/string/split 'static '(r0 r1) '(r1))
(def-method 'compare 'class/string/compare 'static '(r0 r1) '(r1))
(def-method 'same 'class/string/same 'static '(r0 r1) '(r1))
(def-method 'find 'class/string/find 'static '(r0 r1) '(r1))

(def-method 'hash 'class/string/hash 'final)
(def-method 'get_length 'class/string/get_length 'final)
(def-method 'ref_element 'class/string/ref_element 'final)
(def-method 'slice 'class/string/slice 'final)

(def-struct 'string 'sliceable)
	(uint 'string_length)
	(uint 'string_hashcode)
	(offset 'string_data)
(def-struct-end)

(defcvar 'string_gap (sub 64 string_data ptr_size byte_size))

(defcvar 'char_lf 10)
(defcvar 'char_space (ascii " "))
(defcvar 'pipe_char (ascii "|"))
(defcvar 'char_lrb (ascii "("))
(defcvar 'char_rrb (ascii ")"))
(defcvar 'char_lsb (ascii "["))
(defcvar 'char_rsb (ascii "]"))
(defcvar 'char_lcb (ascii "{"))
(defcvar 'char_rcb (ascii "}"))
(defcvar 'char_quote (ascii "'"))
(defcvar 'char_double_quote 34)
(defcvar 'char_tick (ascii "`"))
(defcvar 'char_comma (ascii ","))
(defcvar 'char_tilda (ascii "~"))
(defcvar 'char_semi (ascii ";"))
(defcvar 'char_at (ascii "@"))
(defcvar 'char_equal (ascii "="))
(defcvar 'char_plus (ascii "+"))
(defcvar 'char_minus (ascii "-"))
(defcvar 'char_multiply (ascii "*"))
(defcvar 'char_divide (ascii "/"))
(defcvar 'char_0 (ascii "0"))
(defcvar 'char_9 (ascii "9"))
(defcvar 'char_a (ascii "a"))
(defcvar 'char_z (ascii "z"))
(defcvar 'char_A (ascii "A"))
(defcvar 'char_Z (ascii "Z"))
(defcvar 'char_x (ascii "x"))
(defcvar 'char_b (ascii "b"))
(defcvar 'char_o (ascii "o"))

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
	(vp-cpy-ir-ui r0 string_length r1))
