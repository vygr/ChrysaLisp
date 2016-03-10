%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/hit
		;inputs
		;r0 = view object
		;r8 = x
		;r9 = y
		;outputs
		;r0 = 0, else view object

		if r8, >=, 0
			if r9, >=, 0
				if r8, <, [r0 + view_w]
					if r9, <, [r0 + view_h]
						;hit
						vp_ret
					endif
				endif
			endif
		endif
		vp_xor r0, r0
		vp_ret

	fn_function_end
