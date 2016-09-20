%include 'inc/func.inc'

;%define debug_emit

	def_function test

		ptr set

		push_scope
			assign {@class/string/compare}, {set}
		pop_scope
		return

	def_function_end
