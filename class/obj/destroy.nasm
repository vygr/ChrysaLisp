%include 'inc/func.inc'
%include 'class/class_obj.inc'

	def_function class/obj/destroy
		;inputs
		;r0 = object
		;trashes
		;all but r4

		m_call obj, deinit, {r0}
		m_jmp obj, delete, {r0}

	def_function_end
