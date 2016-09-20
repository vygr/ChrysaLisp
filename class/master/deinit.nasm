%include 'inc/func.inc'
%include 'class/class_master.inc'

	def_function class/master/deinit
		;inputs
		;r0 = master object
		;trashes
		;all but r0, r4

		;call stop
		s_call master, stop, {r0}

		;deinit parent
		p_jmp master, deinit, {r0}

	def_function_end
