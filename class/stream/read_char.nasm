%include 'inc/func.inc'
%include 'class/class_stream.inc'

	def_function class/stream/read_char
		;inputs
		;r0 = stream object
		;outputs
		;r0 = stream object
		;r1 = -1 for EOF, else char read
		;trashes
		;all but r0, r4

		ptr inst
		long char

		push_scope
		retire {r0}, {inst}

		loop_while {inst->stream_bufp == inst->stream_bufe}
			method_call stream, read_next, {inst}, {char}
			gotoif {char == -1}, exit
		loop_end
		assign {*inst->stream_bufp}, {char}
		assign {inst->stream_bufp + 1}, {inst->stream_bufp}
	exit:
		eval {inst, char}, {r0, r1}
		pop_scope
		return

	def_function_end
