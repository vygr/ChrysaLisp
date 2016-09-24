%include 'inc/func.inc'
%include 'class/class_slave.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp_repl

		ptr slave, lisp

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave}
			;create lisp class
			static_call lisp, create, {slave->slave_stdin, slave->slave_stdout, slave->slave_stderr}, {lisp}

			;REPL
			static_call lisp, repl, {lisp, lisp->lisp_stdin}

			;clean up
			static_call lisp, deref, {lisp}
			static_call slave, deref, {slave}
		endif

		pop_scope
		return

	def_function_end
