(include 'sys/list/class.inc)
(include 'class/hmap/class.inc)

(def-class 'component 'hmap)
(dec-method 'vtable 'gui/component/vtable)
(dec-method 'init 'gui/component/init 'static '(r0 r1))
(dec-method 'find_owner 'gui/component/find_owner 'static '(r0) '(r0 r1))
(dec-method 'connect 'gui/component/connect 'static '(r0 r1) '(r0))
(dec-method 'emit 'gui/component/emit 'static '(r0) '(r0))
(dec-method 'get_prop 'gui/component/get_prop 'static '(r0 r1) '(r0 r1))
(dec-method 'ref_prop 'gui/component/ref_prop 'static '(r0 r1) '(r0 r1))
(dec-method 'set_long_prop 'gui/component/set_long_prop 'static '(r0 r1 r2) '(r0))
(dec-method 'get_long_prop 'gui/component/get_long_prop 'static '(r0 r1) '(r0 r1))
(dec-method 'set_font_prop 'gui/component/set_font_prop 'static '(r0 r1 r2 r3) '(r0))
(dec-method 'set_str_prop 'gui/component/set_str_prop 'static '(r0 r1 r2) '(r0))

(dec-method 'deinit 'gui/component/deinit 'override)

(dec-method 'lisp_connect 'gui/component/lisp_connect 'static '(r0 r1) '(r0 r1))

(def-struct 'component 'hmap)
	(ptr 'tcb 'target_ids)
	(long 'id)
(def-struct-end)
