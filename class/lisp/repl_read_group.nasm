%include 'inc/func.ninc'
%include 'inc/load.ninc'
%include 'class/class_string.ninc'
%include 'class/class_stream.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/repl_read_group
	;inputs
	;r0 = lisp object
	;r1 = stream
	;r2 = next char
	;outputs
	;r0 = lisp object
	;r1 = string
	;r2 = next char

	const char_ccurly, '}'

	ptr this, stream, string
	pubyte reloc, buffer
	ulong char

	push_scope
	retire {r0, r1, r2}, {this, stream, char}

	func_path sys_load, statics
	assign {@_function_.ld_statics_reloc_buffer}, {reloc}
	assign {reloc}, {buffer}

	func_call stream, read_char, {stream}, {char}
	loop_while {char != -1 && char != char_ccurly}
		assign {char}, {*buffer}
		assign {buffer + 1}, {buffer}
		func_call stream, read_char, {stream}, {char}
	loop_end
	func_call stream, read_char, {stream}, {char}

	func_call string, create_from_buffer, {reloc, buffer - reloc}, {string}

	expr {this, string, char}, {r0, r1, r2}
	pop_scope
	return

def_func_end
