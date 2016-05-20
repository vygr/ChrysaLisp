%include 'inc/func.inc'

	fn_function test

		const c2, 14
		c1 equ 13

		def_structure test
			long f1
			long f2
			ubyte shared_last_op
		def_structure_end

		ptr p1
		ubyte a1
		pshort a2
		uint i
		int j

		push_scope
			assign {12 + 15 + c1 + c2, a1}, {p1->f1, p1->f2}
			assign {p1->f2, p1->f1}, {p1->f1, p1->f2}
			assign {0}, {p1->shared_last_op}
		pop_scope
		vp_ret

	fn_function_end
