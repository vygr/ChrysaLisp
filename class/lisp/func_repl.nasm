%include 'inc/func.ninc'
%include 'class/class_stream.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_repl
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	const char_lf, 10

	ptr this, args, stream, ast, value
	ulong length, char, flag

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length == 1}
		func_call vector, get_element, {args, 0}, {stream}

		func_call stream, read_char, {stream}, {char}
		loop_start
			virt_call stream, write_flush, {this->lisp_stdout}
			virt_call stream, write_flush, {this->lisp_stderr}

			func_call lisp, repl_read, {this, stream, char}, {ast, char}
			breakif {char == -1}

			vpif {stream == this->lisp_stdin}
				func_call stream, write_cstr, {this->lisp_stdout, "--Ast--"}
				func_call stream, write_char, {this->lisp_stdout, char_lf}
				func_call lisp, repl_print, {this, this->lisp_stdout, ast}
				func_call stream, write_char, {this->lisp_stdout, char_lf}
				func_call stream, write_cstr, {this->lisp_stdout, "--Macro expanding--"}
				func_call stream, write_char, {this->lisp_stdout, char_lf}
			endif

			loop_start
				func_call lisp, repl_expand, {this, &ast}, {flag}
				vpif {stream == this->lisp_stdin}
					func_call lisp, repl_print, {this, this->lisp_stdout, ast}
					func_call stream, write_char, {this->lisp_stdout, 10}
				endif
			loop_until {flag}

			vpif {stream == this->lisp_stdin}
				func_call stream, write_cstr, {this->lisp_stdout, "--Eval--"}
				func_call stream, write_char, {this->lisp_stdout, char_lf}
			endif

			func_call lisp, repl_eval, {this, ast}, {value}
			func_call ref, deref, {ast}

			vpif {value->obj_vtable == @class/class_error}
				func_call lisp, repl_print, {this, this->lisp_stdout, value}
				func_call stream, write_char, {this->lisp_stdout, char_lf}
				loop_start
					func_call stream, available, {stream}, {length}
					breakifnot {length}
					func_call stream, read_char, {stream}, {char}
				loop_end
			elseif {stream == this->lisp_stdin}
				func_call lisp, repl_print, {this, this->lisp_stdout, value}
				func_call stream, write_char, {this->lisp_stdout, char_lf}
			endif

			func_call ref, deref, {value}
		loop_end

		assign {this->lisp_sym_t}, {value}
		func_call ref, ref, {value}
	else
		func_call error, create, {"(repl stream) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
