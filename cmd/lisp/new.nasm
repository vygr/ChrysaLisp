%include 'inc/func.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/new
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;all

		ptr this

		push_scope

		static_call sys_mem, alloc, {lisp_size}, {this, _}
		if {this}
			;clear object memory
			static_call sys_mem, clear, {this, lisp_size}, {_}
		endif

		eval {this}, {r0}
		pop_scope
		return

	def_function_end
