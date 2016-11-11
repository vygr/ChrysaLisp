%include 'inc/func.ninc'
%include 'class/class_stream.ninc'

def_func class/stream/write_next
	;inputs
	;r0 = stream object
	;outputs
	;r0 = stream object
	;trashes
	;all but r0, r4

	v_jmp stream, write_flush, {r0}

def_func_end
