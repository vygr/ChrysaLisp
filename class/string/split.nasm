%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'

	fn_function class/string/split
		;inputs
		;r0 = string object
		;r1 = split char
		;outputs
		;r0 = string object
		;r1 = vector of split strings
		;trashes
		;all but r0, r4

		ptr inst, splits, stream
		ubyte char

		;save inputs
		push_scope
		retire {r0, r1}, {inst, char}

		;create string split
		static_call stream, create, {0, 0, &inst->string_data, inst->string_length}, {stream}
		static_call stream, split, {stream, char}, {splits}
		static_call stream, deref, {stream}

		eval {inst, splits}, {r0, r1}
		pop_scope
		vp_ret

	fn_function_end
