%include 'inc/func.inc'
%include 'class/class_progress.inc'

	def_func class/progress/set_max
		;inputs
		;r0 = progress object
		;r1 = maximum value

		vp_cpy r1, [r0 + progress_max]
		vp_ret

	def_func_end
