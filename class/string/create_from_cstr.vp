%include 'inc/func.ninc'
%include 'inc/string.ninc'
%include 'class/class_string.ninc'

def_func class/string/create_from_cstr
	;inputs
	;r0 = c string pointer
	;outputs
	;r0 = 0 if error, else object
	;trashes
	;r1-r3, r5-r7

	;get size of string
	f_call sys_string, length, {r0}, {r1}
	f_jmp string, create_from_buffer, {r0, r1}, {r0}

def_func_end
