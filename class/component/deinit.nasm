%include 'inc/func.inc'
%include 'class/class_component.inc'

	fn_function class/component/deinit
		;inputs
		;r0 = component object
		;trashes
		;all but r0, r4

		;disconnnect all slots
		s_call component, disconnect_slot, {r0, 0}

		;deinit parent
		p_jmp component, deinit, {r0}

	fn_function_end
