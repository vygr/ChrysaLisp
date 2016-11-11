%include 'inc/func.ninc'
%include 'class/class_obj.ninc'

def_func class/obj/destroy
	;inputs
	;r0 = object
	;trashes
	;all but r4

	v_call obj, deinit, {r0}
	v_jmp obj, delete, {r0}

def_func_end
