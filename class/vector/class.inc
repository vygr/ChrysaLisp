(include 'class/array/class.inc)

(def-class 'vector 'array)
(dec-method 'vtable 'class/vector/vtable)
(dec-method 'create 'class/vector/create)

(dec-method 'deinit 'class/vector/deinit 'final)
(dec-method 'ref_element 'class/vector/ref_element 'final)
(dec-method 'slice 'class/vector/slice 'final)
(dec-method 'cat 'class/vector/cat 'final)
(dec-method 'clear 'class/vector/clear 'final)
(dec-method 'ref_back 'class/vector/ref_back 'final)
(dec-method 'set_element 'class/vector/set_element 'final)
(dec-method 'append 'class/vector/append 'final)

(dec-method 'lisp_list 'class/vector/lisp_list 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_elemset 'class/vector/lisp_elemset 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_merge 'class/vector/lisp_merge 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_part 'class/vector/lisp_part 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_match 'class/vector/lisp_match 'static '(r0 r1) '(r0 r1))

(def-struct 'vector 'array)
(def-struct-end)
