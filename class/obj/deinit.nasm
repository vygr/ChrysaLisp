%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_obj.inc'

	fn_function class/obj/deinit
		;inputs
		;r0 = obj object
		;trashes
		;all but r0, r4

		;disconnnect all slots
		vp_xor r1, r1
		static_jmp obj, disconnect_slot

	fn_function_end
