%include 'inc/func.inc'
%include 'inc/load.inc'

	def_function sys/load_deinit
		;get statics
		s_bind sys_load, statics, r0

		;free all function blocks
		loop_flist_forward r0 + ld_statics_block_flist, r1, r2
			vp_cpy r1, r0
			ln_remove_fnode r1, r2
			sys_munmap r0, ld_block_size
		loop_end
		vp_ret

	def_function_end
