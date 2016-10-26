%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/repl
	;inputs
	;r0 = lisp object
	;r1 = stream to read
	;outputs
	;r0 = lisp object

	const char_lf, 10

	ptr this, stream, ast, value
	ulong char, flag

	push_scope
	retire {r0, r1}, {this, stream}

	func_call stream, read_char, {stream}, {char}
	loop_start
		virt_call stream, write_flush, {this->lisp_stderr}
		func_call sys_task, yield
		virt_call stream, write_flush, {this->lisp_stdout}

		func_call lisp, repl_read, {this, stream, char}, {ast, char}
		breakif {char == -1}

		if {stream == this->lisp_stdin}
			func_call stream, write_cstr, {this->lisp_stdout, "--Ast--"}
			func_call stream, write_char, {this->lisp_stdout, char_lf}
			func_call lisp, repl_print, {this, this->lisp_stdout, ast}
			func_call stream, write_char, {this->lisp_stdout, char_lf}
			func_call stream, write_cstr, {this->lisp_stdout, "--Macro expanding--"}
			func_call stream, write_char, {this->lisp_stdout, char_lf}
		endif

		loop_start
			func_call lisp, repl_expand, {this, &ast}, {flag}
			if {stream == this->lisp_stdin}
				func_call lisp, repl_print, {this, this->lisp_stdout, ast}
				func_call stream, write_char, {this->lisp_stdout, 10}
			endif
		loop_until {flag}

		if {stream == this->lisp_stdin}
			func_call stream, write_cstr, {this->lisp_stdout, "--Eval--"}
			func_call stream, write_char, {this->lisp_stdout, char_lf}
		endif

		func_call lisp, repl_eval, {this, ast}, {value}
		func_call ref, deref, {ast}

		if {stream == this->lisp_stdin}
			func_call lisp, repl_print, {this, this->lisp_stdout, value}
			func_call stream, write_char, {this->lisp_stdout, char_lf}
		endif

		func_call ref, deref, {value}
	loop_end

	eval {this}, {r0}
	pop_scope
	return

def_func_end
