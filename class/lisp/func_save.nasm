%include 'inc/func.ninc'
%include 'inc/syscall.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_string.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_save
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	def_struct pdata
		ptr pdata_this
		ptr pdata_value
		ptr pdata_handle
	def_struct_end

	ptr this, value, handle

	ptr args, name, list
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length == 2}
		func_call vector, get_element, {args, 0}, {list}
		vpif {list->obj_vtable == @class/class_vector}
			func_call vector, get_element, {args, 1}, {name}
			vpif {name->obj_vtable == @class/class_string \
				|| name->obj_vtable == @class/class_symbol}
				devirt_call vector, get_length, {list}, {length}
				func_call sys_io, open, {&name->string_data, o_creat | o_rdwr | o_trunc, s_irusr | s_iwusr | s_irgrp | s_iroth}, {handle}
				vpif {handle >= 3}
					assign {this->lisp_sym_nil}, {value}
					func_call ref, ref, {value}
					func_call vector, for_each, {list, 0, length, $callback, &this}, {_}
					func_call sys_io, close, {handle}
				else
					func_call error, create, {"(save list filename) open error", args}, {value}
				endif
			else
				func_call error, create, {"(save list filename) not filename", args}, {value}
			endif
		else
			func_call error, create, {"(save list filename) not list", args}, {value}
		endif
	else
		func_call error, create, {"(save list filename) wrong number of args", args}, {value}
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
	vpif {(*iter)->obj_vtable == @class/class_boxed_long}
		expr {pdata->pdata_handle, &(*iter)->boxed_ptr_value, 8}, {r7, r6, r2}
		sys_write_string r7, r6, r2
		assign {*iter}, {pdata->pdata_value}
		func_call ref, ref, {pdata->pdata_value}
		expr {1}, {r1}
		return
	else
	error:
		func_call error, create, {"(save list filename) not all numbers", *iter}, {pdata->pdata_value}
	endif

	expr {0}, {r1}
	pop_scope
	return

def_func_end
