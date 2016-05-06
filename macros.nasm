%include 'inc/func.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function 'test'

		;define constants
		const a, 0
		const b, 1
		const c, 2
		const d, 3
		const e, 4
		const f, 5

		;define variables
		byte bbb
		ubyte ubbb
		short sss
		ushort usss
		int iii
		uint uiii
		long lll
		ulong ulll
		struct qqq, long

;		set_token_list zzz
;		print_token_list
;		token_to_rpn
;		print_rpn_list

		vp_ret

	fn_function_end
