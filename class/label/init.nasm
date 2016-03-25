%include 'inc/func.inc'
%include 'inc/font.inc'
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
			vp_lea [rel font_name], r0
			vp_cpy 12, r1
			static_call font, open
			fn_assert r0, !=, 0
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + label_font]
		endif
		vp_pop r0
		vp_ret

	font_name:
		db 'fonts/NimbusRomNo9L-Reg.otf', 0

	fn_function_end
