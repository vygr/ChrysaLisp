%include 'inc/func.inc'
%include 'class/class_flow.inc'
%include 'class/class_string.inc'
%include 'class/class_label.inc'

	fn_function class/label/init
		;inputs
		;r0 = label object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;save object
		vp_push r0

		;init parent
		super_call label, init
		if r1, !=, 0
			;init myself
			;add my flow
			static_call flow, create
			fn_assert r0, !=, 0
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + label_flow]
			vp_cpy flow_flag_down, r8
			static_call flow, set_flags
			vp_xor r8, r8
			vp_xor r9, r9
			vp_xor r10, r10
			vp_xor r11, r11
			static_call flow, set_color
			static_call flow, add

			;add my string
			static_call string, create
			fn_assert r0, !=, 0
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + label_string]
			vp_xor r8, r8
			vp_xor r9, r9
			vp_xor r10, r10
			vp_xor r11, r11
			static_call string, set_color
			vp_cpy [r1 + label_flow], r1
			static_call string, add
		endif
		vp_pop r0
		vp_ret

	fn_function_end
