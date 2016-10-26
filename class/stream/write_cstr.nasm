%include 'inc/func.inc'
%include 'class/class_stream.inc'

	def_func class/stream/write_cstr
		;inputs
		;r0 = stream object
		;r1 = buffer
		;outputs
		;r0 = stream object
		;trashes
		;all but r0, r4

		ptr inst
		pubyte buffer
		long char

		;save inputs
		push_scope
		retire {r0, r1}, {inst, buffer}

		loop_start
			assign {*buffer}, {char}
			breakifnot {char}
			assign {buffer + 1}, {buffer}
			func_call stream, write_char, {inst, char}
		loop_end

		eval {inst}, {r0}
		pop_scope
		return

	def_func_end
