%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/write_char
		;inputs
		;r0 = stream object
		;r1 = char
		;outputs
		;r0 = stream object
		;trashes
		;all but r0, r4

		ptr inst
		long char

		push_scope
		retire {r0, r1}, {inst, char}

		switch
		case {inst->stream_bufp == inst->stream_bufe}
			method_call stream, write_next, {inst}
			breakif {inst->stream_bufp == inst->stream_bufe}
		default
			assign {char}, {*inst->stream_bufp}
			assign {inst->stream_bufp + 1}, {inst->stream_bufp}
		endswitch

		eval {inst}, {r0}
		pop_scope
		return

	fn_function_end
