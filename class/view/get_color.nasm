%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/get_color
		;inputs
		;r0 = view object
		;outputs
		;r0 = view object
		;r8 = red
		;r9 = green
		;r10 = blue
		;r11 = alpha

		;get color info
		vp_cpy [r0 + view_red], r8
		vp_cpy [r0 + view_green], r9
		vp_cpy [r0 + view_blue], r10
		vp_cpy [r0 + view_alpha], r11
		vp_ret

	fn_function_end
