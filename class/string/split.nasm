%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'

	fn_function class/string/split
		;inputs
		;r0 = string object
		;r1 = split char
		;outputs
		;r0 = string object
		;r1 = vector of split strings
		;trashes
		;all but r0, r4

		ptr inst
		ptr stream
		ptr splits
		ptr string
		pubyte start
		ulong length
		ubyte char

		;save inputs
		push_scope
		retire {r0, r1}, {inst, char}

		;create string stream and outout vector
		static_call stream, create_from_string, {inst}, {stream}
		static_call vector, create, {}, {splits}

		;fill vector with splits
		loop_start
			static_call stream, skip, {stream, char}
			assign {stream->stream_bufp}, {start}
			static_call stream, skip_not, {stream, char}
			assign {stream->stream_bufe - start}, {length}
			breakif {length == 0}
			static_call string, create_from_buffer, {start, length}, {string}
			static_call vector, push_back, {splits, string}
		loop_end
		static_call stream, deref, {stream}

		eval {inst, splits}, {r0, r1}
		pop_scope
		vp_ret

	fn_function_end
