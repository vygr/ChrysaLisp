%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_string.inc'
%include 'class/class_vector.inc'

	fn_function class/stream/split
		;inputs
		;r0 = stream object
		;r1 = split char
		;outputs
		;r0 = stream object
		;r1 = vector of split streams
		;trashes
		;all but r0, r4

		ptr inst
		ptr splits
		ptr string
		pubyte start
		ulong length
		ubyte char

		;save inputs
		push_scope
		retire {r0, r1}, {inst, char}

		;create output vector
		static_call vector, create, {}, {splits}

		;fill vector with splits
		loop_start
			static_call stream, skip, {inst, char}
			assign {inst->stream_bufp}, {start}
			static_call stream, skip_not, {inst, char}
			assign {inst->stream_bufp - start}, {length}
			breakif {length == 0}
			static_call string, create_from_buffer, {start, length}, {string}
			static_call vector, push_back, {splits, string}
		loop_end

		eval {inst, splits}, {r0, r1}
		pop_scope
		return

	fn_function_end
