(import 'class/view/view.inc)

(def-class 'flow 'view)
(def-method 'create 'class/flow/create 'static nil '(r0))
(def-method 'new 'class/flow/new 'static nil '(r0))
(def-method 'init 'class/flow/init)
(def-method 'set_flow_flags 'class/flow/set_flow_flags 'static '(r0 r1) '(r0))

(def-method 'pref_size 'class/flow/pref_size 'final)
(def-method 'layout 'class/flow/layout 'final)

(def-bit 'flow_flag)
	(bit 'left 'right 'up 'down
		'fillw 'fillh 'lastw 'lasth
		'align_hcenter 'align_hleft 'align_hright
		'align_vcenter 'align_vtop 'align_vbottom)
(def-bit-end)

(def-struct 'flow 'view)
	(ulong 'flags)
(def-struct-end)
