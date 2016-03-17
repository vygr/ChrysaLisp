%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/set_percent
		;inputs
		;r0 = progress object
		;r1 = percent (0-100)

		vp_cpy r1, [r0 + progress_percent]
		vp_ret

	fn_function_end
