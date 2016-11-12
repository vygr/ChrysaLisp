%include 'inc/func.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/create
	;inputs
	;r0 = stdin stream
	;r1 = stdout stream
	;r2 = stderr stream
	;outputs
	;r0 = 0 if error, else object
	;trashes
	;all

	ptr this, stdin, stdout, stderr
	ulong ok

	push_scope
	retire {r0, r1, r2}, {stdin, stdout, stderr}

	func_call lisp, new, {}, {this}
	if {this != 0}
		;init the object
		func_path class, lisp
		func_call lisp, init, {this, @_function_, stdin, stdout, stderr}, {ok}
		ifnot {ok}
			;error with init
			virt_call lisp, delete, {this}, {}
			assign {0}, {this}
		endif
	endif

	eval {this}, r0
	return

def_func_end
