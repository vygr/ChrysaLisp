%include 'inc/func.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_title.inc'
%include 'class/class_label.inc'

	def_func class/window/init
		;inputs
		;r0 = window object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		s_call window, init, {r0, r1}, {r1}
		if r1, !=, 0
			vp_push r0

			;init myself
			f_call window, set_color, {r0, 0xffc0c0c0}

			;add my flow panel
			f_call flow, create, {}, {r0}
			assert r0, !=, 0
			f_call flow, set_flow_flags, {r0, flow_flag_down | flow_flag_fillw | flow_flag_lasth}
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_flow]
			f_call flow, add_back, {r0, r1}

			;add my title
			f_call title, create, {}, {r0}
			assert r0, !=, 0
			f_call title, set_color, {r0, 0xffc0c0c0}
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_title]
			f_call title, add_back, {r0, [r1 + window_flow]}

			;add my status panel
			f_call flow, create, {}, {r0}
			assert r0, !=, 0
			f_call flow, set_flow_flags, {r0, flow_flag_up | flow_flag_fillw | flow_flag_lasth}
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_panel]
			f_call flow, add_back, {r0, [r1 + window_flow]}

			;add my status label
			f_call label, create, {}, {r0}
			assert r0, !=, 0
			f_call label, set_color, {r0, 0xff808080}
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_status]
			f_call label, add_back, {r0, [r1 + window_panel]}

			vp_pop r0
		endif
		vp_ret

	def_func_end
