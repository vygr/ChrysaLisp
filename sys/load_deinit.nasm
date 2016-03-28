%include 'inc/func.inc'
%include 'inc/load.inc'

	fn_function sys/load_deinit, no_debug_enter
		;get statics
		static_bind sys_load, statics, r0

		;free all function blocks
		loop_flist_forward r0 + ld_statics_block_list, r1, r2
			vp_cpy r1, r0
			remove_fnode r1, r2
			sys_munmap r0, ld_block_size
		loop_end
		vp_ret

	fn_function_end
