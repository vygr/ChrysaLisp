%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_until
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		def_structure pdata
			ptr pdata_this
			ptr pdata_value
		def_structure_end

		struct pdata, pdata
		ptr args
		ulong length

		push_scope
		retire {r0, r1}, {pdata.pdata_this, args}

		assign {0}, {pdata.pdata_value}
		static_call vector, get_length, {args}, {length}
		if {length >= 2}
			loop_start
				static_call vector, get_element, {args, 1}, {pdata.pdata_value}
				static_call lisp, repl_eval, {pdata.pdata_this, pdata.pdata_value}, {pdata.pdata_value}
				breakifnot {pdata.pdata_value}
				breakif {pdata.pdata_value != pdata.pdata_this->lisp_sym_nil}
				static_call vector, for_each, {args, 2, $callback, &pdata}, {_}
				breakifnot {pdata.pdata_value}
				static_call ref, deref, {pdata.pdata_value}
			loop_end
		else
			static_call lisp, error, {pdata.pdata_this, "(until tst form ...) wrong number of args", args}
		endif

		eval {pdata.pdata_this, pdata.pdata_value}, {r0, r1}
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

		static_call ref, deref, {pdata->pdata_value}
		static_call lisp, repl_eval, {pdata->pdata_this, *iter}, {pdata->pdata_value}

		eval {pdata->pdata_value}, {r1}
		pop_scope
		return

	def_function_end
