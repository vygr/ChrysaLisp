%include 'inc/func.inc'
%include 'class/class_view.inc'

	def_function class/view/get_color
		;inputs
		;r0 = view object
		;outputs
		;r0 = view object
		;r1 = color

		;get color info
		vp_cpy [r0 + view_color], r1
		vp_ret

	def_function_end
