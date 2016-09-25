%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/func_cond
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, value
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		static_call lisp, error, {this, "(cond (clause val) (clause val) ...) not done yet", args}

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
