%include 'inc/func.ninc'
%include 'class/class_ref.ninc'

def_func class/ref/ref
	;inputs
	;r0 = object
	;trashes
	;r1

	assert r0, !=, 0

	;inc ref count
	vp_cpy [r0 + ref_count], r1
	vp_inc r1
	vp_cpy r1, [r0 + ref_count]
	vp_ret

def_func_end
