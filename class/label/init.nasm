%include 'inc/func.inc'
%include 'class/class_flow.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	def_function class/label/init
		;inputs
		;r0 = label object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		p_call label, init, {r0, r1}, {r1}
		if r1, !=, 0
			vp_push r0

			;init myself
			;add my flow
			s_call flow, create, {}, {r0}
			assert r0, !=, 0
			s_call flow, set_flags, {r0, 0}
			s_call flow, set_flow_flags, {r0, flow_flag_right | flow_flag_align_vcenter}
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + label_flow]
			s_call flow, add_front, {r0, r1}

			;add my text
			s_call text, create, {}, {r0}
			assert r0, !=, 0
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + label_text]
			s_call text, set_color, {r0, 0}
			vp_cpy [r4], r1
			s_call text, add_front, {r0, [r1 + label_flow]}

			vp_pop r0
		endif
		vp_ret

	def_function_end
