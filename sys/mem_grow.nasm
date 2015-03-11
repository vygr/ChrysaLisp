%include "func.inc"

	fn_function "sys/mem_grow"
		;inputs
		;r0 = array
		;r1 = array size
		;r2 = new array size
		;outputs
		;r0 = new array
		;r1 = new array size
		;trashes
		;r2-r3, r5-r9

		vp_cpy r0, r6
		vp_cpy r1, r7
		if r2, >, r1
			;alloc new table
			vp_cpy r2, r0
			fn_call sys/mem_alloc
			vp_cpy r0, r8
			vp_cpy r1, r9

			;clear it to empty
			for r2, 0, r1, 8
				vp_cpy 0, qword[r0 + r2]
			next

			if r6, !=, 0
				;copy over old data
				vp_cpy r6, r0
				vp_cpy r8, r1
				vp_cpy r7, r2
				fn_call sys/mem_copy

				;free existing
				vp_cpy r6, r0
				fn_call sys/mem_free
			endif
		
			;save new table
			vp_cpy r8, r0
			vp_cpy r9, r1
		endif
		vp_ret

	fn_function_end
