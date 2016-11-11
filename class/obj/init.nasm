%include 'inc/func.ninc'
%include 'class/class_obj.ninc'

def_func class/obj/init
	;inputs
	;r0 = object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok

	;init the object
	vp_cpy r1, [r0 + obj_vtable]
	vp_ret

def_func_end
