%include 'inc/func.ninc'
%include 'class/class_stream.ninc'

def_func class/stream/read_ready
	;inputs
	;r0 = stream object
	;outputs
	;r0 = stream object
	;r1 = 0 if data not available
	;trashes
	;all but r0, r4

	f_jmp stream, available, {r0}

def_func_end
