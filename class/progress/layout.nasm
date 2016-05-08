%include 'inc/func.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/layout
		;inputs
		;r0 = progress object
		;trashes
		;all but r0, r4

		s_jmp progress, opaque, {r0}

	fn_function_end
