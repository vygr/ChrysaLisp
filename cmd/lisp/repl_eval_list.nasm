%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/repl_eval_list
		;inputs
		;r0 = lisp globals
		;r1 = list
		;outputs
		;r0 = 0, else list

		ptr lisp, list
		pptr iter

		push_scope
		retire {r0, r1}, {lisp, list}

		if {list->obj_vtable != @class/class_vector}
			static_call lisp, error, {lisp, "not a list"}
			assign {0}, {list}
		else
			static_call vector, for_each, {list, $repl_eval_list_callback, lisp}, {iter}
			breakif {!iter}
			assign {0}, {list}
		endif

		eval {list}, {r0}
		pop_scope
		return

	repl_eval_list_callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr lisp

		push_scope
		retire {r0, r1}, {iter, lisp}

		static_call lisp, repl_eval, {lisp, *iter}, {lisp}
		if {lisp}
			static_call ref, deref, {*iter}
			assign {lisp}, {*iter}
		endif

		eval {lisp}, {r1}
		pop_scope
		return

	def_function_end
