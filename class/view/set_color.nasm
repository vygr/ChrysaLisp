%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/set_color
		;inputs
		;r0 = view object
		;r8 = red
		;r9 = green
		;r10 = blue
		;r11 = alpha

		;set color info
		vp_cpy r8, [r0 + view_red]
		vp_cpy r9, [r0 + view_green]
		vp_cpy r10, [r0 + view_blue]
		vp_cpy r11, [r0 + view_alpha]
		vp_ret

	fn_function_end
