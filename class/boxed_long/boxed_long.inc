(import 'class/boxed_ptr/boxed_ptr.inc)

(def-class 'boxed_long 'boxed_ptr)
(def-method 'create 'class/boxed_long/create)

(def-struct 'boxed_long 'boxed_ptr)
(def-struct-end)
