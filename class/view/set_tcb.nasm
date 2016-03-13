%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/set_tcb
		;inputs
		;r0 = view object
		;r1 = task tcb

		vp_cpy r1, [r0 + view_tcb]
		vp_ret

	fn_function_end
