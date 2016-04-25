%include 'inc/func.inc'
%include 'class/class_flow.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	fn_function class/label/init
		;inputs
		;r0 = label object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		super_call label, init
		if r1, !=, 0
			vp_push r0

			;init myself
			;add my flow
			static_call flow, create
			assert r0, !=, 0
			static_call flow, set_flags, {r0, 0}
			static_call flow, set_flow_flags, {r0, flow_flag_right | flow_flag_align_vcenter}
			static_call flow, set_color, {r0, 0}
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + label_flow]
			static_call flow, add

			;add my text
			static_call text, create
			assert r0, !=, 0
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + label_text]
			static_call text, set_color, {r0, 0}
			vp_cpy [r4], r1
			static_call text, add, {r0, [r1 + label_flow]}

			vp_pop r0
		endif
		vp_ret

	fn_function_end
