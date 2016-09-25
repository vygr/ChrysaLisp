%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/repl_eval_list
		;inputs
		;r0 = lisp object
		;r1 = list
		;outputs
		;r0 = lisp object
		;r1 = 0, else list

		ptr this, list
		pptr iter

		push_scope
		retire {r0, r1}, {this, list}

		if {list->obj_vtable != @class/class_vector}
			static_call lisp, error, {this, "not a list", list}
			assign {0}, {list}
		else
			static_call vector, for_each, {list, 0, $repl_eval_list_callback, this}, {iter}
			breakif {!iter}
			assign {0}, {list}
		endif

		eval {this, list}, {r0, r1}
		pop_scope
		return

	repl_eval_list_callback:
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
