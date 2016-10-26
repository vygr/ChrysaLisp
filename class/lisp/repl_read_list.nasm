%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/repl_read_list
	;inputs
	;r0 = lisp object
	;r1 = stream
	;r2 = next char
	;outputs
	;r0 = lisp object
	;r1 = list
	;r2 = next char

	const char_space, ' '
	const char_rrb, ')'

	ptr this, stream, list, ast
	ulong char

	push_scope
	retire {r0, r1, r2}, {this, stream, char}

	;skip "(" and white space
	loop_start
		func_call stream, read_char, {stream}, {char}
	loop_until {char > char_space || char == -1}

	func_call vector, create, {}, {list}
	loop_while {char != -1 && char != char_rrb}
		func_call lisp, repl_read, {this, stream, char}, {ast, char}
		if {ast->obj_vtable == @class/class_error}
			func_call ref, deref, {list}
			assign {ast}, {list}
			goto error
		endif
		func_call vector, push_back, {list, ast}

		;skip white space
		loop_while {char <= char_space && char != -1}
			func_call stream, read_char, {stream}, {char}
		loop_end
	loop_end

	;skip ")"
	func_call stream, read_char, {stream}, {char}

error:
	eval {this, list, char}, {r0, r1, r2}
	pop_scope
	return

def_func_end
