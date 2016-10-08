%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_cat
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
		if {length > 1}
			static_call vector, get_element, {args, 1}, {pdata.pdata_value}
			static_call lisp, repl_eval, {pdata.pdata_this, pdata.pdata_value}, {pdata.pdata_value}
			breakifnot {pdata.pdata_value}
			if {pdata.pdata_value->obj_vtable == @class/class_vector \
				|| pdata.pdata_value->obj_vtable == @class/class_string}
				static_call vector, for_each, {args, 2, $callback, &pdata}, {_}
			else
				static_call lisp, error, {pdata.pdata_this, "(cat seq ...) not sequence type", pdata.pdata_value}
				static_call ref, deref, {pdata.pdata_value}
				assign {0}, {pdata.pdata_value}
			endif
		else
			static_call lisp, error, {pdata.pdata_this, "(cat seq ...) wrong number of args", args}
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
		ptr pdata, elem, new_elem
		ulong length

		push_scope
		retire {r0, r1}, {iter, pdata}

		static_call lisp, repl_eval, {pdata->pdata_this, *iter}, {elem}
		if {elem}
			if {elem->obj_vtable == pdata->pdata_value->obj_vtable}
				switch
				case {elem->obj_vtable == @class/class_string}
					static_call string, append, {pdata->pdata_value, elem}, {new_elem}
					static_call ref, deref, {elem}
					static_call ref, deref, {pdata->pdata_value}
					assign {new_elem}, {pdata->pdata_value}
					break
				default
					static_call vector, get_length, {elem}, {length}
					static_call vector, append, {pdata->pdata_value, elem, 0, length}
					static_call ref, deref, {elem}
				endswitch
			else
				static_call lisp, error, {pdata->pdata_this, "(cat seq ...) none matching type", elem}
				static_call ref, deref, {elem}
				goto error1
			endif
		else
		error1:
			static_call ref, deref, {pdata->pdata_value}
			assign {0}, {pdata->pdata_value}
		endif

		eval {pdata->pdata_value}, {r1}
		pop_scope
		return

	def_function_end
