%include 'class/class_obj.inc'

	fn_function 'class/obj/destroy'
		;inputs
		;r0 = object

		method_call obj, deinit
		method_call obj, delete
		vp_ret

	fn_function_end
