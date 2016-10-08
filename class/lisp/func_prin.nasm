%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_prin
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		static_call vector, for_each, {args, 1, $callback, this}, {_}

		eval {this, args}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata

		push_scope
		retire {r0, r1}, {iter, pdata}

		static_call lisp, repl_print, {pdata, pdata->lisp_stdout, *iter}

		eval {1}, {r1}
		pop_scope
		return

	def_function_end
