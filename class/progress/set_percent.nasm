%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/set_percent
		;inputs
		;r0 = progress object
		;r1 = percent
		;outputs
		;r1 = percent, clipped to max

		if r1, >, (1 << progress_fixed_point)
			vp_cpy (1 << progress_fixed_point), r1
		endif
		vp_cpy r1, [r0 + progress_percent]
		vp_ret

	fn_function_end
