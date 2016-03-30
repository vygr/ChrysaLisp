%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/dirty_all
		;inputs
		;r0 = view object
		;trashes
		;all but r0, r4

		;iterate through views
		;dirty all
		vp_rel dirty_down_callback, r2
		vp_rel null_up_callback, r3
		static_jmp view, forward_tree

	dirty_down_callback:
		static_call view, dirty
		vp_xor r1, r1
	null_up_callback:
		vp_ret

	fn_function_end
