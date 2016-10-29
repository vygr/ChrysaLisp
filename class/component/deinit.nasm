%include 'inc/func.inc'
%include 'class/class_component.inc'

def_func class/component/deinit
	;inputs
	;r0 = component object
	;trashes
	;all but r0, r4

	;disconnnect all slots
	f_call component, disconnect_slot, {r0, 0}

	;deinit parent
	s_jmp component, deinit, {r0}

def_func_end
