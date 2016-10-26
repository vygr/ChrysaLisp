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

def_bit
	bit flow_flag_left
	bit flow_flag_right
	bit flow_flag_up
	bit flow_flag_down
	bit flow_flag_fillw
	bit flow_flag_fillh
	bit flow_flag_lastw
	bit flow_flag_lasth
	bit flow_flag_align_hcenter
	bit flow_flag_align_hleft
	bit flow_flag_align_hright
	bit flow_flag_align_vcenter
	bit flow_flag_align_vtop
	bit flow_flag_align_vbottom
def_bit_end

def_structure flow, view
	ulong flow_flags
def_structure_end

%endif
