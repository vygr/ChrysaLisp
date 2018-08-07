(import 'class/view/class.inc)

(def-class 'flow 'view)
(dec-method 'vtable 'class/class_flow)
(dec-method 'create 'class/flow/create 'static '() '(r0))
(dec-method 'new 'class/flow/new)

(dec-method 'pref_size 'class/flow/pref_size 'final)
(dec-method 'layout 'class/flow/layout 'final)

(dec-method 'lisp_create 'class/flow/lisp_create 'static '(r0 r1) '(r0 r1))

(def-bit 'flow_flag)
	(bit 'left 'right 'up 'down
		'fillw 'fillh 'lastw 'lasth
		'align_hcenter 'align_hleft 'align_hright
		'align_vcenter 'align_vtop 'align_vbottom)
(def-bit-end)

(def-struct 'flow 'view)
(def-struct-end)
