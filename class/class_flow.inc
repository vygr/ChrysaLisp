%ifndef CLASS_FLOW_1234
%define CLASS_FLOW_1234

(import 'class/class_view.inc)

(def-class 'flow 'view)
(def-method 'create 'class/flow/create)
(def-method 'new 'class/flow/new)
(def-method 'init 'class/flow/init)
(def-method 'set_flow_flags 'class/flow/set_flow_flags 'static '(r0 r1))

(def-method 'pref_size 'class/flow/pref_size 'override)
(def-method 'layout 'class/flow/layout 'override)

(def-bit)
	(bit 'flow_flag_left)
	(bit 'flow_flag_right)
	(bit 'flow_flag_up)
	(bit 'flow_flag_down)
	(bit 'flow_flag_fillw)
	(bit 'flow_flag_fillh)
	(bit 'flow_flag_lastw)
	(bit 'flow_flag_lasth)
	(bit 'flow_flag_align_hcenter)
	(bit 'flow_flag_align_hleft)
	(bit 'flow_flag_align_hright)
	(bit 'flow_flag_align_vcenter)
	(bit 'flow_flag_align_vtop)
	(bit 'flow_flag_align_vbottom)
(def-bit-end)

(def-struct 'flow 'view)
	(ulong 'flow_flags)
(def-struct-end)

%endif
