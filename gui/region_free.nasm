%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function gui/region_free
		;inputs
		;r0 = region heap pointer
		;r1 = source region listhead pointer
		;trashes
		;r1-r3

		;run through source region list
		vp_cpy [r1], r2
		vp_cpy_cl 0, [r1]
		loop_while r2, !=, 0
			vp_cpy r2, r1
			vp_cpy [r2 + ln_fnode_next], r2
			hp_freecell r0, r1, r3
		loop_end
		vp_ret

	fn_function_end
