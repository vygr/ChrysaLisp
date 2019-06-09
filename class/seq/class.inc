(include 'class/obj/class.inc)

(def-class 'seq 'obj)
(dec-method 'vtable 'class/seq/vtable)
(dec-method 'get_length 'class/obj/null 'virtual '(r0) '(r0 r1))
(dec-method 'ref_element 'class/obj/null 'virtual '(r0 r1) '(r0 r1))
(dec-method 'slice 'class/obj/null 'virtual '(r0 r1 r2) '(r0 r1))
(dec-method 'cat 'class/obj/null 'virtual '(r0 r1) '(r0))

(dec-method 'lisp_length 'class/seq/lisp_length 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_elem 'class/seq/lisp_elem 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_find 'class/seq/lisp_find 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_slice 'class/seq/lisp_slice 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_cat 'class/seq/lisp_cat 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_each 'class/seq/lisp_each 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_some 'class/seq/lisp_some 'static '(r0 r1) '(r0 r1))

(def-struct 'seq 'obj)
(def-struct-end)
