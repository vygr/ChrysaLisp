%include 'inc/func.ninc'
%include 'class/class_symbol.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_ptr.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/repl_eval
	;inputs
	;r0 = lisp object
	;r1 = form
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, form, value, func, args
	ulong length

	push_scope
	retire {r0, r1}, {this, form}

	;evaluate based on type
	assign {form->obj_vtable}, {func}
	switch
	case {func == @class/class_symbol}
		;eval to symbol value
		func_call lisp, env_get, {this, form}, {value}
		break
	case {func == @class/class_vector}
		devirt_call vector, get_length, {form}, {length}
		ifnot {length}
			;eval to nil
			assign {this->lisp_sym_nil}, {value}
			func_call ref, ref, {value}
		else
			;apply function, eval args if needed
			func_call vector, get_element, {form, 0}, {func}
			func_call lisp, repl_eval, {this, func}, {value}
			breakif {value->obj_vtable == @class/class_error}
			assign {value}, {func}
			switch
			case {func->obj_vtable == @class/class_boxed_ptr}
				gotoifnot {func->boxed_ptr_flags}, args_eval_apply
				if {func->boxed_ptr_flags == type_apply}
					func_call lisp, repl_apply, {this, func, form}, {value}
				else ;type_args_apply
					devirt_call vector, slice, {form, 1, length}, {args}
					func_call lisp, repl_apply, {this, func, args}, {value}
					func_call ref, deref, {args}
				endif
				break
			default
			args_eval_apply:
				devirt_call vector, slice, {form, 1, length}, {args}
				func_call lisp, repl_eval_list, {this, args, 0}, {value}
				if {value->obj_vtable != @class/class_error}
					func_call ref, deref, {value}
					func_call lisp, repl_apply, {this, func, args}, {value}
				endif
				func_call ref, deref, {args}
			endswitch
			func_call ref, deref, {func}
		endif
		break
	default
		;eval to self
		assign {form}, {value}
		func_call ref, ref, {value}
	endswitch

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
