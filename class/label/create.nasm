%include 'inc/func.inc'
%include 'class/class_label.inc'

	fn_function class/label/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;create new label object
		static_call label, new, {}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, label
			static_call label, init, {r0, @_function_}, {r1}
			if r1, ==, 0
				;error with init
				method_call label, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
