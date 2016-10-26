%include 'inc/func.inc'

;%define debug_emit

	def_func test

		ptr set

		push_scope
			assign {@class/string/compare}, {set}
		pop_scope
		return

	def_func_end
