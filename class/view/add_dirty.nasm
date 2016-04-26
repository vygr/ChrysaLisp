%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function class/view/add_dirty
		;inputs
		;r0 = view object
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;all but r0, r4

		def_local
			def_local_long	inst
		def_local_end

		;save inputs
		vp_sub	local_size, r4
		vp_cpy r0, .inst

		;paste dirty region
		vp_add r8, r10
		vp_add r9, r11
		vp_lea [r0 + view_dirty_region], r1
		static_bind gui_gui, statics, r0
		vp_add gui_statics_rect_heap, r0
		static_call gui_region, paste_rect

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
