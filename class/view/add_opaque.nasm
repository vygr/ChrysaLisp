%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	def_function class/view/add_opaque
		;inputs
		;r0 = view object
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
		def_structure_end

		;save inputs
		vp_sub	local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		;paste opaque region
		vp_add r8, r10
		vp_add r9, r11
		vp_lea [r0 + view_opaque_region], r1
		s_bind gui_gui, statics, r0
		vp_add gui_statics_rect_heap, r0
		s_call gui_region, paste_rect, {r0, r1, r8, r9, r10, r11}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	def_function_end
