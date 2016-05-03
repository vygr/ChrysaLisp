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
		eval {(a + b) ^ zzz * -xxx / yyy, "test" % 56 * xxx + yyy * yyy}, {r0, r1}

		;define variables
		push_scope
			ushort xxx
			uint yyy
			eval {(a + b) ^ zzz * -xxx / yyy, "test" % xxx * xxx + yyy * yyy}, {r0, r1}
		pop_scope

		;define variables
		push_scope
			byte zzz
			struct qqq, long
			eval {(a + b) ^ zzz * -xxx / yyy, "test" % qqq * :xxx + yyy * @test/path}, {r0, r1}
		pop_scope

	fn_function_end
