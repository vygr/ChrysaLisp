%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_grid.inc'

	fn_function class/grid/pref_size
		;inputs
		;r0 = grid object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_w
			def_long	local_h
		def_structure_end

		vp_sub local_size, r4
		vp_xor r1, r1
		vp_cpy r1, [r4 + local_w]
		vp_cpy r1, [r4 + local_h]

		vp_cpy r4, r1
		vp_rel callback, r2
		static_call grid, backward

		vp_cpy [r4 + local_w], r10
		vp_cpy [r4 + local_h], r11
		vp_mul [r0 + grid_width], r10
		vp_mul [r0 + grid_height], r11
		vp_add local_size, r4
		vp_ret

	callback:
		vp_push r1
		method_call view, pref_size
		vp_pop r1
		if r10, >, [r1 + local_w]
			vp_cpy r10, [r1 + local_w]
		endif
		if r11, >, [r1 + local_h]
			vp_cpy r11, [r1 + local_h]
		endif
		vp_ret

	fn_function_end
