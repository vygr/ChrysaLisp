%include 'inc/func.inc'
%include 'class/class_window.inc'
%include 'class/class_title.inc'

	fn_function class/window/init
		;inputs
		;r0 = window object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;save object
		vp_push r0

		;init parent
		super_call window, init
		if r1, !=, 0
			;init myself
			vp_cpy flow_flag_down | flow_flag_fillw | flow_flag_lasth, r8
			static_call flow, set_flags
			vp_cpy 255, r8
			vp_cpy 255, r9
			vp_cpy 255, r10
			vp_cpy 255, r11
			static_call view, set_color

			;add my title
			static_call title, create
			fn_assert r0, !=, 0
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_title]
			vp_cpy 255, r8
			vp_xor r9, r9
			vp_xor r10, r10
			vp_cpy 255, r11
			static_call view, set_color
			static_call view, add

			;add my panel
			static_call flow, create
			fn_assert r0, !=, 0
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + window_panel]
			vp_cpy flow_flag_fillw | flow_flag_fillh, qword[r0 + flow_flags]
			vp_cpy 255, r8
			vp_cpy 255, r9
			vp_cpy 255, r10
			vp_cpy 255, r11
			static_call view, set_color
			static_call view, add
		endif
		vp_pop r0
		vp_ret

	fn_function_end
