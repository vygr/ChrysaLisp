%include 'inc/func.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_title.inc'
%include 'class/class_label.inc'

	fn_function class/window/init
		;inputs
		;r0 = window object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		super_call window, init
		if r1, !=, 0
			vp_push r0

			;init myself
			static_call window, set_color, 'r0, 0xffc0c0c0'

			;add my flow panel
			static_call flow, create
			assert r0, !=, 0
			static_call flow, set_flow_flags, 'r0, flow_flag_down | flow_flag_fillw | flow_flag_lasth'
			static_call flow, set_color, 'r0, -1'
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_flow]
			static_call flow, add

			;add my title
			static_call title, create
			assert r0, !=, 0
			static_call title, set_color, 'r0, 0xffc0c0c0'
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_title]
			static_call title, add, 'r0, [r1 + window_flow]'

			;add my status panel
			static_call flow, create
			assert r0, !=, 0
			static_call flow, set_flow_flags, 'r0, flow_flag_up | flow_flag_fillw | flow_flag_lasth'
			static_call flow, set_color, 'r0, -1'
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_panel]
			static_call flow, add, 'r0, [r1 + window_flow]'

			;add my status label
			static_call label, create
			assert r0, !=, 0
			static_call label, set_color, 'r0, 0xff808080'
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_status]
			static_call label, add, 'r0, [r1 + window_panel]'

			vp_pop r0
		endif
		vp_ret

	fn_function_end
