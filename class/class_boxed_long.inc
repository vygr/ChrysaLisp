%ifndef CLASS_BOXED_LONG_1234
%define CLASS_BOXED_LONG_1234

(import 'class/class_boxed_ptr.inc)

(def-class 'boxed_long 'boxed_ptr)
(def-method 'create 'class/boxed_long/create)
(def-method 'new 'class/boxed_long/new)

(def-struct 'boxed_long 'boxed_ptr)
(def-struct-end)

%endif
