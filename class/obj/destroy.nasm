%include 'inc/func.inc'
%include 'class/class_obj.inc'

	fn_function class/obj/destroy
		;inputs
		;r0 = object
		;trashes
		;all but r4

		m_call obj, deinit, {r0}
		m_call obj, delete, {r0}
		vp_ret

	fn_function_end
