%include 'inc/func.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/create
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

		static_call lisp, new, {}, {this}
		if {this != 0}
			;init the object
			slot_function class, lisp
			static_call lisp, init, {this, @_function_, stdin, stdout, stderr}, {ok}
			if {!ok}
				;error with init
				method_call lisp, delete, {this}, {}
				assign {0}, {this}
			endif
		endif

		eval {this}, r0
		return

	def_function_end
