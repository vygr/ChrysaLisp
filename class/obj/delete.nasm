%include 'class/class_obj.inc'

	fn_function 'class/obj/delete'
		;inputs
		;r0 = object

		fn_jmp sys/mem_free

	fn_function_end
