(import 'class/view/view.inc)

(def-class 'flow 'view)
(def-method 'create 'class/flow/create 'static nil '(r0))
(def-method 'new 'class/flow/new 'static nil '(r0))
(def-method 'init 'class/flow/init)
(def-method 'set_flow_flags 'class/flow/set_flow_flags 'static '(r0 r1))

(def-method 'pref_size 'class/flow/pref_size 'final)
(def-method 'layout 'class/flow/layout 'final)

(def-bit)
	(bit 'flow_flag_left 'flow_flag_right 'flow_flag_up 'flow_flag_down
		'flow_flag_fillw 'flow_flag_fillh 'flow_flag_lastw 'flow_flag_lasth
		'flow_flag_align_hcenter 'flow_flag_align_hleft 'flow_flag_align_hright
		'flow_flag_align_vcenter 'flow_flag_align_vtop 'flow_flag_align_vbottom)
(def-bit-end)

(def-struct 'flow 'view)
	(ulong 'flow_flags)
(def-struct-end)
