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
			vp_cpy 0xffc0c0c0, r1
			static_call window, set_color

			;add my flow panel
			static_call flow, create
			fn_assert r0, !=, 0
			vp_cpy flow_flag_down | flow_flag_fillw | flow_flag_lasth, r1
			static_call flow, set_flow_flags
			vp_cpy 0xffffffff, r1
			static_call flow, set_color
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_flow]
			static_call flow, add

			;add my title
			static_call title, create
			fn_assert r0, !=, 0
			vp_cpy 0xffc0c0c0, r1
			static_call title, set_color
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_title]
			vp_cpy [r1 + window_flow], r1
			static_call title, add

			;add my status panel
			static_call flow, create
			fn_assert r0, !=, 0
			vp_cpy flow_flag_up | flow_flag_fillw | flow_flag_lasth, r1
			static_call flow, set_flow_flags
			vp_cpy 0xffffffff, r1
			static_call flow, set_color
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_panel]
			vp_cpy [r1 + window_flow], r1
			static_call flow, add

			;add my status label
			static_call label, create
			fn_assert r0, !=, 0
			vp_cpy 0xff808080, r1
			static_call label, set_color
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_status]
			vp_cpy [r1 + window_panel], r1
			static_call label, add

			vp_pop r0
		endif
		vp_ret

	fn_function_end
