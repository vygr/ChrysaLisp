%include 'inc/func.ninc'
%include 'class/class_flow.ninc'

def_func class/flow/init
	;inputs
	;r0 = flow object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok

	;init parent
	s_call flow, init, {r0, r1}, {r1}
	vpif r1, !=, 0
		;init myself
		vp_xor r2, r2
		vp_cpy r2, [r0 + flow_flags]
	endif
	vp_ret

def_func_end
