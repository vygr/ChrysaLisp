%include 'inc/func.inc'
%include 'class/class_stream.inc'

	def_func class/stream/read_next
		;inputs
		;r0 = stream object
		;outputs
		;r0 = stream object
		;r1 = -1 for EOF, else more data
		;trashes
		;all but r0, r4

		vp_cpy -1, r1
		vp_ret

	def_func_end
