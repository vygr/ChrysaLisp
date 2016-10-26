%include 'inc/func.inc'
%include 'inc/load.inc'
%include 'inc/string.inc'
%include 'class/class_stream.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/repl_read_num
	;inputs
	;r0 = lisp object
	;r1 = stream
	;r2 = next char
	;outputs
	;r0 = lisp object
	;r1 = number
	;r2 = next char

	const char_0, '0'
	const char_9, '9'
	const char_minus, '-'

	ptr this, stream, num
	pubyte reloc, buffer
	ulong char, sign
	long val

	push_scope
	retire {r0, r1, r2, r2}, {this, stream, char, sign}

	if {char == char_minus}
		func_call stream, read_char, {stream}, {char}
	endif

	func_path sys_load, statics
	assign {@_function_.ld_statics_reloc_buffer}, {reloc}
	assign {reloc}, {buffer}

	loop_while {char >= char_0 && char <= char_9}
		assign {char}, {*buffer}
		assign {buffer + 1}, {buffer}
		func_call stream, read_char, {stream}, {char}
	loop_end
	assign {0}, {*buffer}

	;create the number
	func_call boxed_long, create, {}, {num}
	func_call sys_string, to_long, {reloc, 10}, {val}
	if {sign == char_minus}
		assign {-val}, {val}
	endif
	func_call boxed_long, set_value, {num, val}

	eval {this, num, char}, {r0, r1, r2}
	pop_scope
	return

def_func_end
