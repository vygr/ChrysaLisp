%include 'class/class_obj.inc'

	fn_function 'class/obj/init'
		;inputs
		;r0 = object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init the object
		vp_cpy r1, [r0 + OBJ_VTABLE]
		vp_ret

	fn_function_end
