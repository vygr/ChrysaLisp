(import 'class/obj/class.inc)

(def-class 'sequence 'obj)
(dec-method 'vtable 'class/class_sequence)
(dec-method 'get_length 'class/obj/null 'virtual '(r0) '(r0 r1))
(dec-method 'ref_element 'class/obj/null 'virtual '(r0 r1) '(r0 r1))
(dec-method 'slice 'class/obj/null 'virtual '(r0 r1 r2) '(r0 r1))

(def-struct 'sequence 'obj)
(def-struct-end)
