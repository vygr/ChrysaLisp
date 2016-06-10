%include 'inc/func.inc'

%define debug_emit

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
		pop_scope
		vp_ret

	fn_function_end
