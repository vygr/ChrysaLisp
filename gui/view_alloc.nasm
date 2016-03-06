%include 'inc/func.inc'
%include 'inc/list.inc'
%include 'inc/gui.inc'

	fn_function gui/view_alloc
		;outputs
		;r0 = view object
		;trashes
		;r1-r3

		;alloc view objects
		static_bind gui, statics, r0
		vp_lea [r0 + gui_statics_view_heap], r0
		static_call heap, alloc
		vp_cpy r1, r0

		;init child list etc
		vp_cpy 0, qword[r0 + gui_view_parent]
		vp_lea [r0 + gui_view_list], r1
		lh_init r1, r2
		vp_cpy 0, qword[r0 + gui_view_dirty_list]
		vp_cpy 0, qword[r0 + gui_view_transparent_list]

		;init draw callback to null
		vp_lea [rel myret], r1
		vp_cpy r1, [r0 + gui_view_draw]
	myret:
		vp_ret

	fn_function_end
