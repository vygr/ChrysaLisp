%include 'inc/func.ninc'
%include 'class/class_stream.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

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
	const char_semi, ';'
	const char_lf, 10

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

		;skip white space/comments
		loop_start
			loop_while {char <= char_space && char != -1}
				func_call stream, read_char, {stream}, {char}
			loop_end
			breakif {char != char_semi}
			func_call stream, skip_not, {stream, char_lf}
			func_call stream, read_char, {stream}, {char}
		loop_end
	loop_end

	;skip ")"
	func_call stream, read_char, {stream}, {char}

error:
	expr {this, list, char}, {r0, r1, r2}
	pop_scope
	return

def_func_end
