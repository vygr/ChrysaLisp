%include 'inc/func.inc'

	fn_function sys/mem_grow, no_debug_enter
		;inputs
		;r0 = array
		;r1 = array size
		;r2 = new array size
		;outputs
		;r0 = new array
		;r1 = new array size
		;trashes
		;r2-r3, r5-r8

		vp_cpy r0, r5
		vp_cpy r1, r6
		if r2, >, r1
			;alloc new table
			static_call sys_mem, alloc, {r2}, {r0, r1}
			assert r0, !=, 0
			vp_cpy r0, r7
			vp_cpy r1, r8

			;clear it to empty
			static_call sys_mem, clear, {r0, r1}

			if r6, !=, 0
				;copy over old data
				static_call sys_mem, copy, {r5, r7, r6}, {r0, r1}

				;free existing
				static_call sys_mem, free, {r5}
			endif

			;save new table
			vp_cpy r7, r0
			vp_cpy r8, r1
		endif
		vp_ret

	fn_function_end
