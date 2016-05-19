%include 'inc/func.inc'

	fn_function test

		ubyte a1
		pshort a2
		uint i
		int j

		push_scope
			eval {a1[i], a2[j]}, {r0, r1}
		pop_scope
		vp_ret

	fn_function_end
