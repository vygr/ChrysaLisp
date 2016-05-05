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

;		set_token_list @zzz >> :xxx + "bert"
;		print_token_list
;		token_to_rpn
;		print_rpn_list

		;define variables
		push_scope
			assign {bbb + 100}, {iii}

			;define variables
			ushort xxx
			uint yyy
			push_scope
;				eval {(a + b) ^ zzz * - xxx / yyy, "test" % xxx * xxx + yyy * yyy}, {r0, r1}
			pop_scope

			;define variables
			struct qqq, long
			byte zzz

			push_scope
;				eval {(a + b) ^ zzz * - xxx / yyy, "test" % qqq * :xxx + yyy * @test/path}, {r0, r1}
			pop_scope
		pop_scope
		vp_ret

	fn_function_end
