%include 'inc/func.ninc'
%include 'class/class_string.ninc'
%include 'class/class_stream.ninc'

def_func class/string/split
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
	func_call stream, create, {0, 0, &inst->string_data, inst->string_length}, {stream}
	func_call stream, split, {stream, char}, {splits}
	func_call stream, deref, {stream}

	expr {inst, splits}, {r0, r1}
	pop_scope
	return

def_func_end
