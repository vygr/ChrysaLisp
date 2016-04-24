%include 'inc/func.inc'
%include 'inc/list.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

%define dual_buffers

	fn_function gui/gui_update
		;inputs
		;r0 = root view object
		;trashes
		;all but r4

		def_structure	local
			def_struct	local_ctx, gui_ctx
		def_structure_end

		vp_sub local_size, r4
		static_bind gui_gui, statics, r1
		vp_cpy [r1 + gui_statics_renderer], r1
		vp_cpy r1, [r4 + local_ctx + gui_ctx_sdl_ctx]

		;iterate through views back to front
		;setting abs cords
		vp_xor r8, r8
		vp_xor r9, r9
		vp_rel abs_down_callback, r2
		vp_rel abs_up_callback, r3
		static_call view, backward_tree, 'r0, r1, r2, r3'

		;iterate through views back to front
		;create visible region at root
		vp_rel null_func_down_callback, r2
		vp_rel visible_up_callback, r3
		static_call view, backward_tree, 'r0, r0, r2, r3'

%ifdef dual_buffers
		;copy visable region to new region
		vp_push r0, 0
		vp_lea [r0 + view_dirty_region], r1
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		static_bind gui_gui, statics, r0
		vp_lea [r0 + gui_statics_rect_heap], r0
		static_call gui_region, copy_rect, 'r0, r1, r4, 0, 0, r10, r11'

		;paste old visable region into root
		vp_cpy [r4 + 8], r0
		vp_lea [r0 + view_dirty_region], r2
		static_bind gui_gui, statics, r1
		vp_lea [r1 + gui_statics_rect_heap], r0
		vp_add gui_statics_old_region, r1
		static_call gui_region, paste_region, 'r0, r1, r2, 0, 0'

		;free old region and splice over new region
		static_bind gui_gui, statics, r5
		vp_lea [r5 + gui_statics_rect_heap], r0
		vp_lea [r5 + gui_statics_old_region], r1
		static_call gui_region, free
		vp_pop r1
		vp_cpy r1, [r5 + gui_statics_old_region]
		vp_pop r0
%endif

		;iterate through views front to back
		;distribute visible region
		vp_rel null_func_down_callback, r2
		vp_rel distribute_up_callback, r3
		static_call view, forward_tree, 'r0, r0, r2, r3'

		;iterate through views back to front
		;drawing each view
		vp_lea [r4 + local_ctx], r1
		vp_rel draw_down_callback, r2
		vp_rel null_func_up_callback, r3
		static_call view, backward_tree

		vp_add local_size, r4
		vp_ret

	null_func_down_callback:
		vp_cpy r0, r1
	null_func_up_callback:
		vp_ret

	abs_down_callback:
		vp_add [r0 + view_x], r8
		vp_add [r0 + view_y], r9
		vp_cpy r8, [r0 + view_ctx_x]
		vp_cpy r9, [r0 + view_ctx_y]
		vp_cpy r0, r1
		vp_ret

	abs_up_callback:
		vp_sub [r0 + view_x], r8
		vp_sub [r0 + view_y], r9
		vp_ret

	visible_up_callback:
		vp_push r1, r0

		;region heap
		static_bind gui_gui, statics, r0
		vp_lea [r0 + gui_statics_rect_heap], r0

		;remove opaque region from parent if not root
		vp_cpy [r4], r1
		if r1, !=, [r4 + 8]
			vp_cpy [r1 + view_x], r8
			vp_cpy [r1 + view_y], r9
			vp_cpy [r1 + view_parent], r2
			vp_add view_opaque_region, r1
			vp_add view_dirty_region, r2
			static_call gui_region, remove_region
		endif

		;clip local dirty region with parent bounds if not root
		vp_cpy [r4], r1
		vp_cpy [r1 + view_parent], r2
		if r1, ==, [r4 + 8]
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
		vp_add view_dirty_region, r1
		static_call gui_region, clip_rect

		;paste local dirty region onto parent if not root
		vp_cpy [r4], r1
		if r1, !=, [r4 + 8]
			vp_cpy [r1 + view_x], r8
			vp_cpy [r1 + view_y], r9
			vp_cpy [r1 + view_parent], r2
			vp_add view_dirty_region, r1
			vp_add view_dirty_region, r2
			static_call gui_region, paste_region

			;free local dirty region
			vp_cpy [r4], r1
			vp_add view_dirty_region, r1
			static_call gui_region, free
		endif

		vp_pop r1, r0
		vp_ret

	distribute_up_callback:
		vp_push r1, r0

		;region heap
		static_bind gui_gui, statics, r0
		vp_lea [r0 + gui_statics_rect_heap], r0

		;copy view from root if not root
		vp_cpy [r4], r2
		vp_cpy [r4 + 8], r1
		if r2, !=, r1
			vp_cpy [r2 + view_ctx_x], r8
			vp_cpy [r2 + view_ctx_y], r9
			vp_cpy [r2 + view_w], r10
			vp_cpy [r2 + view_h], r11
			vp_add r8, r10
			vp_add r9, r11
			vp_add view_dirty_region, r1
			vp_add view_dirty_region, r2
			static_call gui_region, copy_rect

			;remove opaque region
			vp_cpy [r4], r1
			vp_cpy [r4 + 8], r2
			vp_cpy [r1 + view_ctx_x], r8
			vp_cpy [r1 + view_ctx_y], r9
			vp_add view_opaque_region, r1
			vp_add view_dirty_region, r2
			static_call gui_region, remove_region
		endif

		vp_pop r1, r0
		vp_ret

	draw_down_callback:
		vp_lea [r0 + view_dirty_region], r2
		vp_cpy [r2], r3
		if r3, !=, 0
			vp_push r0

			;draw myself
			vp_cpy [r0 + view_ctx_x], r8
			vp_cpy [r0 + view_ctx_y], r9
			vp_cpy r8, [r1 + gui_ctx_x]
			vp_cpy r9, [r1 + gui_ctx_y]
			vp_cpy r2, [r1 + gui_ctx_dirty_region]
			method_call view, draw

			;free local dirty region
			vp_cpy [r4], r1
			vp_add view_dirty_region, r1
			static_bind gui_gui, statics, r0
			vp_lea [r0 + gui_statics_rect_heap], r0
			static_call gui_region, free

			vp_pop r0
		endif
		vp_cpy r0, r1
		vp_ret

	fn_function_end
