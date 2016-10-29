%ifndef CLASS_COMPONENT_1234
%define CLASS_COMPONENT_1234

%include 'class/class_ref.inc'

def_class component, ref
def_method init, class/component/init
def_method connect, class/component/connect, static, {r0, r1, r2, r3}
def_method emit, class/component/emit, static, {r0, r1}
def_method disconnect_slot, class/component/disconnect_slot, static, {r0, r1}
def_method disconnect_sig, class/component/disconnect_sig, static, {r0, r1}

def_method deinit, class/component/deinit, override

def_struct component, ref
	struct component_slot_list, lh_list
def_struct_end

%endif
