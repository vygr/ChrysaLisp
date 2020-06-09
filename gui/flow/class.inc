(include 'gui/view/class.inc)

(def-class 'flow 'view)
(dec-method 'vtable 'gui/flow/vtable)
(dec-method 'create 'gui/flow/create :static '() '(r0))

(dec-method 'pref_size 'gui/flow/pref_size :final)
(dec-method 'layout 'gui/flow/layout :final)

(dec-method 'lisp_create 'gui/flow/lisp_create :static '(r0 r1) '(r0 r1))

(def-bit 'flow_flag)
	(bit 'left 'right 'up 'down
		'fillw 'fillh 'lastw 'lasth
		'align_hcenter 'align_hleft 'align_hright
		'align_vcenter 'align_vtop 'align_vbottom)
(def-bit-end)

(def-struct 'flow 'view)
(def-struct-end)
