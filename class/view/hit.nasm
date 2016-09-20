%include 'inc/func.inc'
%include 'class/class_view.inc'

	def_function class/view/hit
		;inputs
		;r0 = view object
		;r8 = x
		;r9 = y
		;outputs
		;r0 = view object
		;r1 = 0 if not, else hit

		if r8, >=, 0
			if r9, >=, 0
				if r8, <, [r0 + view_w]
					if r9, <, [r0 + view_h]
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

	def_function_end
