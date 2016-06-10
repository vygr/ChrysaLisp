%include 'inc/func.inc'

%define debug_emit

	fn_function test

		ptr abc
		ptr def
		ptr xyz
		ptr qwe

		push_scope
			assign {abc, def}, {xyz, qwe}
		pop_scope
		vp_ret

	fn_function_end
