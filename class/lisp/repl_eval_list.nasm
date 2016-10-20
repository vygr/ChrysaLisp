%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_eval_list
		;inputs
		;r0 = lisp object
		;r1 = list
		;r2 = start index
		;outputs
		;r0 = lisp object
		;r1 = 0, else list

		ptr this, list
		pptr iter
		ulong index

		push_scope
		retire {r0, r1, r2}, {this, list, index}

		static_call vector, for_each, {list, index, $callback, this}, {iter}
		if {iter}
			assign {0}, {list}
		endif

		eval {this, list}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata

		push_scope
		retire {r0, r1}, {pdata, iter}

		static_call lisp, repl_eval, {pdata, *iter}, {pdata}
		if {pdata}
			static_call ref, deref, {*iter}
			assign {pdata}, {*iter}
		endif

		eval {pdata}, {r1}
		pop_scope
		return

	def_function_end
