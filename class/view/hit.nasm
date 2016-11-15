%include 'inc/func.ninc'
%include 'class/class_view.ninc'

def_func class/view/hit
	;inputs
	;r0 = view object
	;r8 = x
	;r9 = y
	;outputs
	;r0 = view object
	;r1 = 0 if not, else hit

	vpif r8, >=, 0
		vpif r9, >=, 0
			vpif r8, <, [r0 + view_w]
				vpif r9, <, [r0 + view_h]
					;hit ?
					vp_cpy [r0 + view_flags], r1
					vp_and view_flag_solid, r1
					vp_ret
				endif
			endif
		endif
	endif
	vp_xor r1, r1
	vp_ret

def_func_end
