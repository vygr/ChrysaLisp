%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_component.inc'

	fn_function class/component/deinit
		;inputs
		;r0 = component object
		;trashes
		;all but r0, r4

		;disconnnect all slots
		vp_xor r1, r1
		static_call component, disconnect_slot, {r0, r1}

		;deinit parent
		super_jmp component, deinit, {r0}

	fn_function_end
