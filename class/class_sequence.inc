%ifndef CLASS_SEQUENCE_1234
%define CLASS_SEQUENCE_1234

(import 'class/class_ref.inc)

(def-class 'sequence 'ref)
(def-method 'get_length 'class/obj/null 'virtual '(r0) '(r1))
(def-method 'ref_element 'class/obj/null 'virtual '(r0 r1) '(r1))
(def-method 'slice 'class/obj/null 'virtual '(r0 r1 r2) '(r1))

(def-struct 'sequence 'ref)
(def-struct-end)

%endif
