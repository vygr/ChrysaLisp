%include 'inc/func.ninc'
%include 'class/class_unordered_map.ninc'
%include 'class/class_pair.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/repl_expand
	;inputs
	;r0 = lisp object
	;r1 = iter to form
	;outputs
	;r0 = lisp object
	;r1 = 0 if expanded

	def_struct pdata
		ptr pdata_this
		ptr pdata_form
	def_struct_end

	pptr iter, miter
	ptr this, form, macro, args
	ulong length

	push_scope
	retire {r0, r1}, {this, iter}

	assign {*iter}, {form}
	vpif {form->obj_vtable == @class/class_vector}
		devirt_call vector, get_length, {form}, {length}
		vpif {length}
			func_call vector, get_element, {form, 0}, {macro}
			breakif {macro == this->lisp_sym_qquote}
			breakif {macro == this->lisp_sym_quote}
			vpif {macro->obj_vtable == @class/class_symbol}
				func_call unordered_map, find, {this->lisp_macros, macro}, {miter, _}
				vpif {miter}
					func_call pair, get_second, {*miter}, {macro}
					func_call lisp, env_push, {this}
					func_call vector, get_element, {macro, 0}, {args}
					func_call lisp, env_bind, {this, args, form, 1}, {form}
					vpif {form->obj_vtable != @class/class_error}
						func_call vector, for_each, {macro, 1, macro->vector_length, $callback, &this}, {_}
					endif
					func_call lisp, env_pop, {this}
					func_call ref, deref, {*iter}
					assign {form}, {*iter}
					expr {this, form->obj_vtable == @class/class_error}, {r0, r1}
					return
				endif
			endif
			func_path lisp, repl_expand
			func_call vector, for_each, {form, 0, length, @_function_, this}, {iter}
			assign {!iter}, {iter}
		endif
	endif

	expr {this, iter}, {r0, r1}
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

	func_call ref, deref, {pdata->pdata_form}
	func_call lisp, repl_eval, {pdata->pdata_this, *iter}, {pdata->pdata_form}

	expr {pdata->pdata_form->obj_vtable != @class/class_error}, {r1}
	pop_scope
	return

def_func_end
