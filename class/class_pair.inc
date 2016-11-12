%ifndef CLASS_PAIR_1234
%define CLASS_PAIR_1234

(import 'class/class_sequence.inc)

(def-class 'pair 'sequence)
(def-method 'create 'class/pair/create 'static '(r0 r1) '(r0))
(def-method 'new 'class/pair/new)
(def-method 'init 'class/pair/init 'static '(r0 r1 r2 r3) '(r1))
(def-method 'ref_first 'class/pair/ref_first 'static '(r0) '(r1))
(def-method 'ref_second 'class/pair/ref_second 'static '(r0) '(r1))
(def-method 'get_first 'class/pair/get_first 'static '(r0) '(r1))
(def-method 'get_second 'class/pair/get_second 'static '(r0) '(r1))
(def-method 'set_second 'class/pair/set_second 'static '(r0 r1))

(def-method 'deinit 'class/pair/deinit 'override)
(def-method 'get_length 'class/pair/get_length 'override)
(def-method 'ref_element 'class/pair/ref_element 'override)
(def-method 'slice 'class/pair/slice 'override)

(def-struct 'pair 'sequence)
	(ptr 'pair_first)
	(ptr 'pair_second)
(def-struct-end)

%endif
