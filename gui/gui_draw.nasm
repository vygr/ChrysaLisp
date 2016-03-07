%include 'inc/func.inc'
%include 'inc/list.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	struc draw_view
		draw_view_ctx:			resb gui_ctx_size
		draw_view_root:			resq 1
		draw_view_node:			resq 1
		draw_view_patch_list:	resq 1
	endstruc

	fn_function gui/gui_draw
		;inputs
		;r0 = view object
		;trashes
		;all but r4

		vp_sub draw_view_size, r4
		vp_cpy r0, [r4 + draw_view_root]
		static_bind gui, statics, r1
		vp_cpy [r1 + gui_statics_renderer], r1
		vp_cpy r1, [r4 + draw_view_ctx + gui_ctx_sdl_ctx]
		vp_cpy 0, qword[r4 + draw_view_patch_list]

		;iterate through views back to front
		;setting abs cords
		vp_xor r8, r8
		vp_xor r9, r9
		vp_lea [rel abs_down_callback], r2
		vp_lea [rel abs_up_callback], r3
		static_call view, forward

		;iterate through views back to front
		;create visible region list
		vp_lea [r4 + draw_view_patch_list], r1
		vp_lea [rel visible_down_callback], r2
		vp_lea [rel null_func], r3
		static_call view, forward

		;iterate through views front to back
		;distribute visible region list
		vp_lea [r4 + draw_view_patch_list], r1
		vp_lea [rel null_func], r2
		vp_lea [rel distribute_up_callback], r3
		static_call view, backward

		;any remaining patches are root views
		vp_cpy r0, r1
		vp_cpy [r4 + draw_view_patch_list], r0
		vp_cpy r0, [r1 + view_dirty_list]
		vp_cpy 0, qword[r4 + draw_view_patch_list]

		;iterate through views back to front drawing
		vp_cpy [r4 + draw_view_root], r0
		vp_lea [r4 + draw_view_ctx], r1
		vp_lea [rel draw_down_callback], r2
		vp_lea [rel null_func], r3
		static_call view, backward

		vp_add draw_view_size, r4
	null_func:
		vp_ret

	abs_down_callback:
		vp_add [r0 + view_x], r8
		vp_add [r0 + view_y], r9
		vp_cpy r8, [r0 + view_ctx_x]
		vp_cpy r9, [r0 + view_ctx_y]
		vp_ret

	abs_up_callback:
		vp_sub [r0 + view_x], r8
		vp_sub [r0 + view_y], r9
		vp_ret

	visible_down_callback:
		;save node
		vp_push r0, r1

		;region heap
		static_bind gui, statics, r0
		vp_lea [r0 + gui_statics_rect_heap], r0

		;if opaque view remove from global dirty list
		vp_cpy [r4 + 8], r1
		vp_cpy [r1 + view_transparent_list], r2
		if r2, ==, 0
			vp_cpy [r1 + view_ctx_x], r8
			vp_cpy [r1 + view_ctx_y], r9
			vp_cpy [r1 + view_w], r10
			vp_cpy [r1 + view_h], r11
			vp_add r8, r10
			vp_add r9, r11
			vp_cpy [r4], r1
			static_call region, remove
		endif

		;clip local dirty list with parent bounds
		vp_cpy [r4 + 8], r1
		vp_cpy [r1 + view_parent], r2
		if r2, ==, 0
			vp_cpy r1, r2
		endif
		vp_cpy [r1 + view_x], r8
		vp_cpy [r1 + view_y], r9
		vp_cpy [r2 + view_w], r10
		vp_cpy [r2 + view_h], r11
		vp_mul -1, r8
		vp_mul -1, r9
		vp_add r8, r10
		vp_add r9, r11
		vp_add view_dirty_list, r1
		static_call region, clip

		;paste local dirty list onto global dirty list
		vp_cpy [r4 + 8], r2
		vp_cpy [r2 + view_ctx_x], r8
		vp_cpy [r2 + view_ctx_y], r9
		vp_cpy [r4], r2
		static_call region, paste_region

		;free local dirty list
		vp_cpy [r4 + 8], r1
		vp_lea [r1 + view_dirty_list], r1
		static_call region, free

		vp_pop r0, r1
		vp_ret

	distribute_up_callback:
		;if opaque cut view else copy transparent patches
		vp_cpy r0, r3
		vp_cpy [r0 + view_ctx_x], r8
		vp_cpy [r0 + view_ctx_y], r9
		vp_lea [r0 + view_dirty_list], r2
		static_bind gui, statics, r0
		vp_lea [r0 + gui_statics_rect_heap], r0
		if qword[r3 + view_transparent_list], ==, 0
			vp_cpy [r3 + view_w], r10
			vp_cpy [r3 + view_h], r11
			vp_add r8, r10
			vp_add r9, r11
			static_call region, cut
		else
			vp_add view_transparent_list, r3
			static_call region, copy_region
		endif
		vp_ret

	draw_down_callback:
		;draw myself
		vp_push r0
		vp_lea [r0 + view_dirty_list], r2
		vp_cpy [r0 + view_ctx_x], r8
		vp_cpy [r0 + view_ctx_y], r9
		vp_cpy r2, [r1 + gui_ctx_dirty_region]
		vp_cpy r8, [r1 + gui_ctx_x]
		vp_cpy r9, [r1 + gui_ctx_y]
		method_call view, draw

		;free local dirty list
		vp_pop r1
		vp_add view_dirty_list, r1
		static_bind gui, statics, r0
		vp_lea [r0 + gui_statics_rect_heap], r0
		static_call region, free
		vp_ret

	fn_function_end
