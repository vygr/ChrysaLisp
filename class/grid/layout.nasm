%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_grid.inc'

	fn_function class/grid/layout
		;inputs
		;r0 = grid object
		;trashes
		;all but r0, r4

		def_structure local
			long local_count
			long local_cell_w
			long local_cell_h
		def_structure_end

		vp_sub local_size, r4
		vp_xor r1, r1
		vp_cpy r1, [r4 + local_count]

		vp_xor r10, r10
		vp_cpy [r0 + view_w], r9
		vp_shl 32, r9
		vp_cpy [r0 + grid_width], r8
		vp_div r8, r10, r9
		vp_cpy r9, [r4 + local_cell_w]

		vp_xor r10, r10
		vp_cpy [r0 + view_h], r9
		vp_shl 32, r9
		vp_cpy [r0 + grid_height], r8
		vp_div r8, r10, r9
		vp_cpy r9, [r4 + local_cell_h]

		s_call grid, backward, {r0, r4, $callback}

		vp_add local_size, r4
		vp_ret

	callback:
		vp_xor r8, r8
		vp_cpy [r1 + local_count], r9
		vp_cpy [r0 + view_parent], r2
		vp_cpy [r2 + grid_width], r10
		vp_div r10, r8, r9
		vp_cpy [r1 + local_cell_w], r10
		vp_cpy [r1 + local_cell_h], r11
		vp_mul r10, r8
		vp_mul r11, r9
		vp_add r8, r10
		vp_add r9, r11
		vp_shr 32, r8
		vp_shr 32, r9
		vp_shr 32, r10
		vp_shr 32, r11
		vp_sub r8, r10
		vp_sub r9, r11

		vp_cpy [r1 + local_count], r2
		vp_inc r2
		vp_cpy r2, [r1 + local_count]
		s_jmp view, change, {r0, r8, r9, r10, r11}

	fn_function_end
