(include 'class/array/class.inc)

(def-class 'vector 'array)
(dec-method 'vtable 'class/vector/vtable)
(dec-method 'create 'class/vector/create)

(dec-method 'deinit 'class/vector/deinit 'final)
(dec-method 'ref_element 'class/vector/ref_element 'final)
(dec-method 'slice 'class/vector/slice 'final)
(dec-method 'clear 'class/vector/clear 'final)
(dec-method 'ref_back 'class/vector/ref_back 'final)
(dec-method 'pop_back 'class/vector/pop_back 'final)
(dec-method 'set_element 'class/vector/set_element 'final)
(dec-method 'append 'class/vector/append 'final)

(def-struct 'vector 'array)
(def-struct-end)
