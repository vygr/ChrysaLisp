%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_cond
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		def_structure pdata
			ptr pdata_this
			ptr pdata_value
		def_structure_end

		struct pdata, pdata
		ptr args

		push_scope
		retire {r0, r1}, {pdata.pdata_this, args}

		assign {pdata.pdata_this->lisp_sym_nil}, {pdata.pdata_value}
		static_call ref, ref, {pdata.pdata_value}
		static_call vector, for_each, {args, 1, $callback, &pdata}, {_}

		eval {pdata.pdata_this, pdata.pdata_value}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata, test
		ulong length

		push_scope
		retire {r0, r1}, {pdata, iter}

		if {(*iter)->obj_vtable == @class/class_vector}
			slot_call vector, get_length, {*iter}, {length}
			if {length}
				static_call vector, get_element, {*iter, 0}, {test}
				static_call lisp, repl_eval, {pdata->pdata_this, test}, {test}
				if {test->obj_vtable == @class/class_error}
					static_call ref, deref, {pdata->pdata_value}
					assign {test}, {pdata->pdata_value}
					eval {0}, {r1}
					return
				endif
				if {test != pdata->pdata_this->lisp_sym_nil}
					static_call ref, deref, {test}
					static_call vector, for_each, {*iter, 1, $callback1, pdata}, {_}
					eval {0}, {r1}
					return
				else
					static_call ref, deref, {test}
					eval {1}, {r1}
					return
				endif
			else
				static_call ref, deref, {pdata->pdata_value}
				static_call error, create, {"(cond (tst form ...) ...) clause wrong number of args", *iter}, {pdata->pdata_value}
			endif
		else
			static_call ref, deref, {pdata->pdata_value}
			static_call error, create, {"(cond (tst form ...) ...) clause not list", *iter}, {pdata->pdata_value}
		endif

		eval {0}, {r1}
		pop_scope
		return

	callback1:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata

		push_scope
		retire {r0, r1}, {pdata, iter}

		static_call ref, deref, {pdata->pdata_value}
		static_call lisp, repl_eval, {pdata->pdata_this, *iter}, {pdata->pdata_value}

		eval {pdata->pdata_value->obj_vtable != @class/class_error}, {r1}
		pop_scope
		return

	def_function_end
