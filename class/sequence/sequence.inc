(import 'class/obj/obj.inc)

(def-class 'sequence 'obj)
(def-method 'vtable 'class/class_sequence)
(def-method 'get_length 'class/obj/null 'virtual '(r0) '(r0 r1))
(def-method 'ref_element 'class/obj/null 'virtual '(r0 r1) '(r0 r1))
(def-method 'slice 'class/obj/null 'virtual '(r0 r1 r2) '(r0 r1))

(def-struct 'sequence 'obj)
(def-struct-end)
