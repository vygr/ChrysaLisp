%include 'inc/func.inc'
%include 'class/class_view.inc'

	def_function class/view/opaque
		;inputs
		;r0 = view object
		;trashes
		;all but r0, r4

		;remove any opaque region
		s_call view, sub_opaque, {r0, -1000000, -1000000, 2000000, 2000000}

		;paste opaque region
		s_jmp view, add_opaque, {r0, 0, 0, [r0 + view_w], [r0 + view_h]}

	def_function_end
