%include 'inc/func.ninc'
%include 'class/class_stream.ninc'
%include 'class/class_string.ninc'
%include 'class/class_vector.ninc'

def_func class/stream/split
	;inputs
	;r0 = stream object
	;r1 = split char
	;outputs
	;r0 = stream object
	;r1 = vector of split streams
	;trashes
	;all but r0, r4

	ptr inst, splits, string
	pubyte start
	ulong length
	ubyte char

	;save inputs
	push_scope
	retire {r0, r1}, {inst, char}

	;create output vector
	func_call vector, create, {}, {splits}

	;fill vector with splits
	loop_start
		func_call stream, skip, {inst, char}
		assign {inst->stream_bufp}, {start}
		func_call stream, skip_not, {inst, char}
		assign {inst->stream_bufp - start}, {length}
		breakif {length == 0}
		func_call string, create_from_buffer, {start, length}, {string}
		func_call vector, push_back, {splits, string}
	loop_end

	eval {inst, splits}, {r0, r1}
	pop_scope
	return

def_func_end
