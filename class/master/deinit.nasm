%include 'inc/func.inc'
%include 'class/class_master.inc'

	def_func class/master/deinit
		;inputs
		;r0 = master object
		;trashes
		;all but r0, r4

		;call stop
		f_call master, stop, {r0}

		;deinit parent
		s_jmp master, deinit, {r0}

	def_func_end
