%include 'inc/func.inc'
%include 'class/class_stream.inc'

	def_function class/stream/read_line
		;inputs
		;r0 = stream object
		;r1 = buffer
		;r2 = buffer length
		;outputs
		;r0 = stream object
		;r1 = -1 for EOF, else bytes read
		;trashes
		;all but r0, r4

		ptr inst
		pubyte buffer, buffer_end, start
		long char

		;save inputs
		push_scope
		retire {r0, r1, r2}, {inst, buffer, buffer_end}

		if {buffer_end == 0}
			eval {inst, 0}, {r0, r1}
			return
		endif

		assign {buffer, buffer + buffer_end}, {start, buffer_end}
		loop_start
			static_call stream, read_char, {inst}, {char}
			continueif {char == 13}
			breakif {char == 10 || char < 0}
			assign {char}, {*buffer}
			assign {buffer + 1}, {buffer}
		loop_until {buffer == buffer_end}

		if {buffer == start && char < 0}
			eval {inst, char}, {r0, r1}
		else
			eval {inst, buffer - start}, {r0, r1}
		endif
		pop_scope
		return

	def_function_end
