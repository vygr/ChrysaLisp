%include 'inc/func.inc'
%include 'class/class_stream.inc'

	def_function class/stream/write_next
		;inputs
		;r0 = stream object
		;outputs
		;r0 = stream object
		;trashes
		;all but r0, r4

		m_jmp stream, write_flush, {r0}

	def_function_end
