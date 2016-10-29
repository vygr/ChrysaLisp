%include 'inc/func.inc'

;%define debug_emit

	def_func test

		ulong a, b

		push_scope
			assign {a % 2}, {b}
		pop_scope
		return

	def_func_end
