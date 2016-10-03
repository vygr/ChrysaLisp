%include 'inc/func.inc'
%include 'class/class_symbol.inc'

	def_function class/symbol/create_from_cstr
		;inputs
		;r0 = c symbol pointer
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5-r7

		static_call string, create_from_cstr, {r0}, {r0}
		if r0, !=, 0
			slot_function class, symbol
			fn_bind _function_, r1
			vp_cpy r1, [r0 + obj_vtable]
		endif
		vp_ret

	def_function_end
