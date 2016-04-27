%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/dirty_all
		;inputs
		;r0 = view object
		;trashes
		;all but r0, r4

		;iterate through views
		;dirty all
		static_jmp view, forward_tree, {r0, r1, $dirty_down_callback, $null_up_callback}

	dirty_down_callback:
		static_call view, dirty, {r0}
		vp_xor r1, r1
	null_up_callback:
		vp_ret

	fn_function_end
