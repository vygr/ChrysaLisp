(include 'class/obj/class.inc)

(def-class 'sequence 'obj)
(dec-method 'vtable 'class/sequence/vtable)
(dec-method 'get_length 'class/obj/null 'virtual '(r0) '(r0 r1))
(dec-method 'ref_element 'class/obj/null 'virtual '(r0 r1) '(r0 r1))
(dec-method 'slice 'class/obj/null 'virtual '(r0 r1 r2) '(r0 r1))
(dec-method 'cat 'class/obj/null 'virtual '(r0 r1) '(r0))

(dec-method 'lisp_length 'class/sequence/lisp_length 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_elem 'class/sequence/lisp_elem 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_find 'class/sequence/lisp_find 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_slice 'class/sequence/lisp_slice 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_cat 'class/sequence/lisp_cat 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_each 'class/sequence/lisp_each 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_some 'class/sequence/lisp_some 'static '(r0 r1) '(r0 r1))

(def-struct 'sequence 'obj)
(def-struct-end)
