(import 'class/view/view.inc)

(def-class 'flow 'view)
(def-method 'create 'class/flow/create 'static '() '(r0))
(def-method 'new 'class/flow/new)

(def-method 'pref_size 'class/flow/pref_size 'final)
(def-method 'layout 'class/flow/layout 'final)

(def-bit 'flow_flag)
	(bit 'left 'right 'up 'down
		'fillw 'fillh 'lastw 'lasth
		'align_hcenter 'align_hleft 'align_hright
		'align_vcenter 'align_vtop 'align_vbottom)
(def-bit-end)

(def-struct 'flow 'view)
(def-struct-end)
