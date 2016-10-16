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

		if {list->obj_vtable == @class/class_vector}
			static_call vector, for_each, {list, index, $callback, this}, {iter}
			breakifnot {iter}
			assign {0}, {list}
		else
			static_call lisp, error, {this, "not a list", list}
			assign {0}, {list}
		endif

		eval {this, list}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr this

		push_scope
		retire {r0, r1}, {iter, this}

		static_call lisp, repl_eval, {this, *iter}, {this}
		if {this}
			static_call ref, deref, {*iter}
			assign {this}, {*iter}
		endif

		eval {this}, {r1}
		pop_scope
		return

	def_function_end
