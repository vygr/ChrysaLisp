%ifndef CLASS_REF_1234
%define CLASS_REF_1234

(import 'class/class_obj.inc)

(def-class 'ref 'obj)
(def-method 'init 'class/ref/init)
(def-method 'ref 'class/ref/ref 'static '(r0))
(def-method 'deref 'class/ref/deref 'static '(r0))
(def-method 'ref_if 'class/ref/ref_if 'static '(r0))
(def-method 'deref_if 'class/ref/deref_if 'static '(r0))

(def-struct 'ref 'obj)
	ulong ref_count
(def-struct-end)

%endif
