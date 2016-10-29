%include 'inc/func.inc'
%include 'class/class_symbol.inc'

def_func class/symbol/append
	;inputs
	;r0 = symbol object
	;r1 = symbol object
	;outputs
	;r0 = 0 if error, else new symbol object
	;trashes
	;r1-r3, r5-r7

	func_call string, append, {r0, r1}, {r0}
	if r0, !=, 0
		func_path class, symbol
		fn_bind _function_, r1
		vp_cpy r1, [r0 + obj_vtable]
	endif
	vp_ret

def_func_end
