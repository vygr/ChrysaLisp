%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_cond
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

		assign {0}, {value}
		static_call lisp, error, {this, "(cond (tst exp) (tst exp) ...) not done yet", args}

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
