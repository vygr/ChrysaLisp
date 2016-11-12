%ifndef CLASS_STRING_1234
%define CLASS_STRING_1234

(import 'class/class_sequence.inc)

(def-class 'string 'sequence)
(def-method 'create_from_buffer 'class/string/create_from_buffer 'static '(r0, r1), '(r0))
(def-method 'create_from_cstr 'class/string/create_from_cstr 'static '(r0), '(r0))
(def-method 'create_from_file 'class/string/create_from_file 'static '(r0), '(r0))
(def-method 'create_from_long 'class/string/create_from_long 'static '(r0, r1), '(r0))
(def-method 'append 'class/string/append 'static '(r0, r1), '(r0))
(def-method 'new 'class/string/new 'static '(r0), '(r0))
(def-method 'init 'class/string/init 'static '(r0, r1, r2, r3), '(r1))
(def-method 'init1 'class/string/init1 'static '(r0, r1, r2, r3), '(r1))
(def-method 'init2 'class/string/init2 'static '(r0, r1, r2, r3), '(r1))
(def-method 'split 'class/string/split 'static '(r0, r1), '(r1))
(def-method 'compare 'class/string/compare 'static '(r0, r1), '(r1))

(def-method 'hash 'class/string/hash 'override)
(def-method 'get_length 'class/string/get_length 'override)
(def-method 'ref_element 'class/string/ref_element 'override)
(def-method 'slice 'class/string/slice 'override)

(def-struct 'string 'sequence)
	ulong string_length
	ulong string_hashcode
	offset string_data
(def-struct-end)

%endif
