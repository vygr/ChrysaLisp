(import 'class/array/array.inc)

(def-class 'vector 'array)
(def-method 'vtable 'class/class_vector)
(def-method 'create 'class/vector/create)

(def-method 'deinit 'class/vector/deinit 'final)
(def-method 'ref_element 'class/vector/ref_element 'final)
(def-method 'slice 'class/vector/slice 'final)
(def-method 'clear 'class/vector/clear 'final)
(def-method 'ref_back 'class/vector/ref_back 'final)
(def-method 'pop_back 'class/vector/pop_back 'final)
(def-method 'set_element 'class/vector/set_element 'final)
(def-method 'append 'class/vector/append 'final)

(def-struct 'vector 'array)
(def-struct-end)
