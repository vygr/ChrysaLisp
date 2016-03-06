%include 'inc/func.inc'
%include 'inc/load.inc'

	fn_function sys/load_deinit, no_debug_enter
		;get statics
		static_bind load, statics, r0

		;free all function blocks
		vp_cpy [r0 + ld_statics_block_list], r1
		loop_start
			breakif r1, ==, 0
			vp_cpy [r1], r3
			sys_munmap r1, ld_block_size
			vp_cpy r3, r1
		loop_end
		vp_ret

	fn_function_end
