%ifndef CLASS_FLOW_1234
%define CLASS_FLOW_1234

%include 'class/class_view.inc'

	def_class flow, view
	def_method create, class/flow/create
	def_method new, class/flow/new
	def_method set_flags, class/flow/set_flags

	def_method pref_size, class/flow/pref_size, override
	def_method layout, class/flow/layout, override

	flow_flag_left	equ 1 << 0
	flow_flag_right	equ 1 << 1
	flow_flag_up	equ 1 << 2
	flow_flag_down	equ 1 << 3
	flow_flag_fillw	equ 1 << 4
	flow_flag_fillh	equ 1 << 5
	flow_flag_lastw	equ 1 << 6
	flow_flag_lasth	equ 1 << 7

	def_structure flow, view
		def_long	flow_flags
	def_structure_end

%endif
