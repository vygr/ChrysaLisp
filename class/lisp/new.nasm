%include 'inc/func.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/new
	;outputs
	;r0 = 0 if error, else object
	;trashes
	;all

	ptr this

	push_scope

	func_call sys_mem, alloc, {lisp_size}, {this, _}
	vpif {this}
		;clear object memory
		func_call sys_mem, clear, {this, lisp_size}, {_}
	endif

	expr {this}, {r0}
	pop_scope
	return

def_func_end
