%include 'inc/func.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function 'test'

		;define constants
		def_const a, 0
		def_const b, 1
		def_const c, 2
		def_const d, 3
		def_const e, 4
		def_const f, 5

		;define variables
		short xxx
		int yyy
		long zzz

		;define variables
		push_vars
			eval {(a + b) ^ zzz * -xxx / yyy, "test" % 56 * xxx + yyy * yyy}, {r0, r1}

			;define variables
			ushort xxx
			uint yyy
			push_vars
				eval {(a + b) ^ zzz * -xxx / yyy, "test" % xxx * xxx + yyy * yyy}, {r0, r1}
			pop_vars

			;define variables
			byte zzz
			struct qqq, long
			push_vars
				eval {(a + b) ^ zzz * -xxx / yyy, "test" % qqq * :xxx + yyy * @test/path}, {r0, r1}
			pop_vars
		pop_vars
		vp_ret

	fn_function_end
