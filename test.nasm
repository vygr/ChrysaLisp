%include 'inc/func.inc'

;%define debug_emit

	fn_function test

		long abc
		long def
		long xyz
		long qwe

		push_scope
			assign {abc / def}, {xyz}
			assign {abc % def}, {xyz}
			assign {abc // def}, {xyz}
			assign {abc %% def}, {xyz}
		pop_scope
		return

	fn_function_end
