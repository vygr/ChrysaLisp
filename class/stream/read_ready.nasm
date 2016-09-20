%include 'inc/func.inc'
%include 'class/class_stream.inc'

	def_function class/stream/read_ready
		;inputs
		;r0 = stream object
		;outputs
		;r0 = stream object
		;r1 = 0 if data not available
		;trashes
		;all but r0, r4

		s_jmp stream, available, {r0}

	def_function_end
