%include 'inc/func.ninc'
%include 'class/class_boxed_ptr.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/repl_apply
	;inputs
	;r0 = lisp object
	;r1 = function
	;r2 = ast
	;outputs
	;r0 = lisp object
	;r1 = value

	def_struct pdata
		ptr pdata_this
		ptr pdata_value
	def_struct_end

	ptr this, value, func, ast

	push_scope
	retire {r0, r1, r2}, {this, func, ast}

	if {func->obj_vtable == @class/class_boxed_ptr}
		;built in or compiled function
		expr {this, ast, func}, {r0, r1, r2}
		vp_call [r2 + boxed_ptr_value]
		retire {r1}, {value}
	elseif {func->obj_vtable == @class/class_vector}
		;lambda
		ptr vars
		ulong length
		push_scope
		devirt_call vector, get_length, {func}, {length}
		if {length > 2}
			func_call vector, get_element, {func, 0}, {vars}
			if {vars == this->lisp_sym_lambda}
				func_call lisp, env_push, {this}
				func_call vector, get_element, {func, 1}, {vars}
				func_call lisp, env_bind, {this, vars, ast, 0}, {value}
				if {value->obj_vtable != @class/class_error}
					func_call vector, for_each, {func, 2, length, $callback, &this}, {_}
				endif
				func_call lisp, env_pop, {this}
			else
				func_call error, create, {"(lambda vars body) not lambda", vars}, {value}
			endif
		else
			func_call error, create, {"(lambda vars body) wrong numbers of args", func}, {value}
		endif
		pop_scope
	else
		func_call error, create, {"(lambda vars body) not a lambda list", func}, {value}
	endif

	expr {this, value}, {r0, r1}
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

	func_call ref, deref, {pdata->pdata_value}
	func_call lisp, repl_eval, {pdata->pdata_this, *iter}, {pdata->pdata_value}

	expr {pdata->pdata_value->obj_vtable != @class/class_error}, {r1}
	pop_scope
	return

def_func_end
