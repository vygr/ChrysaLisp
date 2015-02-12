%include "func.inc"
%include "load.inc"

	fn_function "sys/load_function_load"
		;input
		;r0 = function path name
		;output
		;r0 = 0 else, function entry pointer
		;trashes
		;r1-r3, r5-r8

	ld_load_function:
		;save pathname
		vp_cpy r0, r7

		;get loader statics !
		vp_lea [rel __func_start], r0
		vp_add [r0 + FN_HEADER_LENGTH], r0
		vp_add [r0 + FN_HEADER_ENTRY], r0
		vp_call r0
		vp_cpy r0, r8

		;check if function allready present !
		vp_xor r5, r5
		vp_cpy [r8 + LD_STATICS_FUNCTION_LIST], r6
		repeat
			breakif r6, ==, 0
			vp_cpy r7, r0
			vp_lea [r6 + FN_HEADER_PATHNAME], r1
			vp_call string_compare
			if r0, !=, 0
				vp_cpy r6, r5
				vp_add [r6 + FN_HEADER_ENTRY], r5
			endif
			vp_cpy [r6], r6
		until r5, !=, 0
		if r5, !=, 0
			;found function allready loaded
			vp_cpy r5, r0
			vp_ret
		endif

		;get length of function on disk
		vp_lea [r8 + LD_STATICS_STAT_BUFFER], r0
		sys_stat r7, r0
		if r0, !=, 0
			;no such file
			xor r0, r0
			vp_ret
		endif

		;ensure space for new function
		vp_cpy [r8 + LD_STATICS_BLOCK_START], r1
		vp_cpy [r8 + LD_STATICS_BLOCK_END], r2
		vp_sub r1, r2
		vp_lea [r8 + LD_STATICS_STAT_BUFFER], r0
		vp_cpy [r0 + STAT_FSIZE], r0
		if r2, <, r0
			;not enough so allcate new function buffer
			sys_mmap 0, LD_BLOCK_SIZE, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0

			;add to block list for freeing
			vp_cpy [r8 + LD_STATICS_BLOCK_LIST], r3
			vp_cpy r3, [r0]
			vp_cpy r0, [r8 + LD_STATICS_BLOCK_LIST]

			;set block pointers for loading
			vp_add 8, r0
			vp_cpy r0, [r8 + LD_STATICS_BLOCK_START]
			vp_add LD_BLOCK_SIZE - 8, r0
			vp_cpy r0, [r8 + LD_STATICS_BLOCK_END]
		endif

		;open function file
		sys_open r7, O_RDONLY, 0
		vp_cpy r0, r12

		;read into buffer
		vp_cpy [r8 + LD_STATICS_BLOCK_START], r3
		vp_lea [r8 + LD_STATICS_STAT_BUFFER], r2
		sys_read r12, r3, [r2 + STAT_FSIZE]

		;close function file
		sys_close r12

		;add to function list
		vp_cpy [r8 + LD_STATICS_FUNCTION_LIST], r0
		vp_cpy r0, [r3]
		vp_cpy r3, [r8 + LD_STATICS_FUNCTION_LIST]

		;adjust block start
		vp_cpy r3, r0
		vp_add [r2 + STAT_FSIZE], r0
		vp_cpy r0, [r8 + LD_STATICS_BLOCK_START]

		;load and link function references
		vp_cpy r3, r0
		vp_add [r3 + FN_HEADER_LINKS], r0
		loopstart
			vp_cpy [r0], r1
			breakif r1, ==, 0
			vp_lea [r0 + 8], r0
			vp_push r0
			vp_push r3
			vp_call ld_load_function
			vp_cpy r0, r1
			vp_pop r3
			vp_pop r0
			vp_cpy r1, [r0 - 8]
			repeat
				vp_cpy byte[r0], r1l
				vp_inc r0
				vp_and 0xff, r1
			until r1, ==, 0
			vp_add 7, r0
			vp_and -8, r0
		loopend

		;return function address
		vp_cpy r3, r0
		vp_add [r3 + FN_HEADER_ENTRY], r0
		vp_ret

	string_compare:
		loopstart
			vp_cpy byte[r0], r2l
			vp_cpy byte[r1], r3l
			vp_and 0xff, r2
			vp_and 0xff, r3
			breakif r2, !=, r3
			if r2, ==, 0
				vp_cpy 1, r0
				vp_ret
			endif
			vp_inc r0
			vp_inc r1
		loopend
		vp_xor r0, r0
		vp_ret

	fn_function_end
