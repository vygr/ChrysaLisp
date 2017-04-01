(import 'class/sequence/sequence.inc)

(def-class 'sliceable 'sequence)
(def-method 'slice 'class/obj/null 'virtual '(r0 r1 r2) '(r0 r1))

(def-struct 'sliceable 'sequence)
(def-struct-end)
