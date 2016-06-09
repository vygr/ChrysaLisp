%include 'inc/func.inc'
%include 'class/class_master.inc'

	fn_function class/master/deinit
		;inputs
		;r0 = master object
		;trashes
		;all but r0, r4

		;call stop
		s_call master, stop, {r0}

		;deinit parent
		p_jmp master, deinit, {r0}

	fn_function_end
