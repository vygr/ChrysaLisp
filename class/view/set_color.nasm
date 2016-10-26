%include 'inc/func.inc'
%include 'class/class_view.inc'

	def_func class/view/set_color
		;inputs
		;r0 = view object
		;r1 = color

		;set color info
		vp_cpy r1, [r0 + view_color]
		vp_ret

	def_func_end
