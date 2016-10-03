%include 'inc/func.inc'
%include 'class/class_symbol.inc'

	def_function class/symbol/create_from_long
		;inputs
		;r0 = number
		;r1 = base
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;all but r4

		static_call string, create_from_long, {r0, r1}, {r0}
		if r0, !=, 0
			slot_function class, symbol
			fn_bind _function_, r1
			vp_cpy r1, [r0 + obj_vtable]
		endif
		vp_ret

	def_function_end
