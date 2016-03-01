%include "inc/func.inc"

	fn_function "sys/mem_grow"
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
			vp_cpy r2, r0
			fn_call sys/mem_alloc
			vp_cpy r0, r7
			vp_cpy r1, r8

			;clear it to empty
			fn_call sys/mem_clear

			if r6, !=, 0
				;copy over old data
				vp_cpy r5, r0
				vp_cpy r7, r1
				vp_cpy r6, r2
				fn_call sys/mem_copy

				;free existing
				vp_cpy r5, r0
				fn_call sys/mem_free
			endif

			;save new table
			vp_cpy r7, r0
			vp_cpy r8, r1
		endif
		vp_ret

	fn_function_end
