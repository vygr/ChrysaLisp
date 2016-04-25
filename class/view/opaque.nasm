%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/opaque
		;inputs
		;r0 = view object
		;trashes
		;all but r0, r4

		;paste opaque region
		static_jmp view, add_opaque, {r0, 0, 0, [r0 + view_w], [r0 + view_h]}

	fn_function_end
