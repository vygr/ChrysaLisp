%include 'inc/func.inc'

def_func gui/ctx_brighter
	;inputs
	;r1 = colour
	;inputs
	;r1 = brighter colour
	;trashes
	;r2, r3

	vp_cpy r1, r2
	vp_cpy 0xff000000, r3
	vp_and r3, r1
	vp_and 0x00fefefe, r2
	vp_shr 1, r2
	vp_add 0x00808080, r2
	vp_add r2, r1
	vp_ret

def_func_end
