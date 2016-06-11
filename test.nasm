%include 'inc/func.inc'

	fn_function test

		ptr abc
		ptr def
		ptr xyz
		ptr qwe

		push_scope
			retire {r0, r1, r2, r3}, {abc, def, xyz, qwe}
			retire {r0, r1, r2, r3}, {_, def, _, qwe}
			eval {abc, def, xyz, qwe}, {r0, r1, _, r3}
			assign {abc, def}, {_, qwe}

			eval {xyz, def}, {r0, r1}
			return

			ulong v1
			ulong v2

			push_scope
				eval {xyz, v2}, {r0, r1}
				return
			pop_scope

		eval {abc, qwe}, {r0, r1}
		pop_scope
		return

	fn_function_end
