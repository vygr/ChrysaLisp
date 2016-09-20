%include 'inc/func.inc'
%include 'class/class_progress.inc'

	def_function class/progress/layout
		;inputs
		;r0 = progress object
		;trashes
		;all but r0, r4

		s_jmp progress, opaque, {r0}

	def_function_end
