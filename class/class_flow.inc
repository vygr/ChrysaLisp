%ifndef CLASS_FLOW_1234
%define CLASS_FLOW_1234

%include 'class/class_view.inc'

	def_class flow, view
	def_method create, class/flow/create
	def_method new, class/flow/new
	def_method init, class/flow/init
	def_method set_flow_flags, class/flow/set_flow_flags, static, {r0, r1}

	def_method pref_size, class/flow/pref_size, override
	def_method layout, class/flow/layout, override

	flow_flag_left			equ 1 << 0
	flow_flag_right			equ 1 << 1
	flow_flag_up			equ 1 << 2
	flow_flag_down			equ 1 << 3
	flow_flag_fillw			equ 1 << 4
	flow_flag_fillh			equ 1 << 5
	flow_flag_lastw			equ 1 << 6
	flow_flag_lasth			equ 1 << 7
	flow_flag_align_hcenter	equ 1 << 8
	flow_flag_align_hleft	equ 1 << 9
	flow_flag_align_hright	equ 1 << 10
	flow_flag_align_vcenter	equ 1 << 11
	flow_flag_align_vtop	equ 1 << 12
	flow_flag_align_vbottom	equ 1 << 13

	def_structure	flow, view
		long	flow_flags
	def_structure_end

%endif
