%include 'inc/func.inc'

;%define debug_emit

	fn_function test

		ptr set

		push_scope
			assign {@class/string/compare}, {set}
		pop_scope
		return

	fn_function_end
