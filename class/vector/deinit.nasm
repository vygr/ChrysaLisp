%include 'inc/func.inc'
%include 'class/class_vector.inc'

def_func class/vector/deinit
	;inputs
	;r0 = vector object
	;trashes
	;all but r0, r4

	f_call vector, clear, {r0}
	s_jmp vector, deinit, {r0}

def_func_end
