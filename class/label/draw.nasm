%include 'inc/func.inc'
%include 'class/class_label.inc'

	fn_function class/label/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
			def_long	local_ctx
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]
		vp_cpy r1, [r4 + local_ctx]

		;draw panel
		vp_cpy 1, r2
		vp_cpy label_border_size, r3
		static_call label, draw_panel

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
