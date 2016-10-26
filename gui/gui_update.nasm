%include 'inc/func.inc'
%include 'inc/list.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

%define dual_buffers

def_func gui/gui_update
	;inputs
	;r0 = root view object
	;trashes
	;all but r4

	def_structure local
		ptr local_root
		ptr local_ctx_flist
		ptr local_ctx_next
		struct local_ctx, gui_ctx
	def_structure_end

	;save inputs
	vp_sub local_size, r4
	vp_cpy r0, [r4 + local_root]

	;iterate through views back to front
	;setting abs cords
	vp_xor r8, r8
	vp_xor r9, r9
	f_call view, backward_tree, {r0, r0, $abs_down_callback, $abs_up_callback}

	;iterate through views back to front
	;create visible region at root
	f_call view, backward_tree, {r0, r0, $null_func_down_callback, $visible_up_callback}

%ifdef dual_buffers
	;copy visable region to new region
	vp_push r0, 0
	vp_lea [r0 + view_dirty_region], r1
	vp_cpy [r0 + view_w], r10
	vp_cpy [r0 + view_h], r11
	f_bind gui_gui, statics, r0
	vp_add gui_statics_rect_heap, r0
	f_call gui_region, copy_rect, {r0, r1, r4, 0, 0, r10, r11}

	;paste old visable region into root
	vp_cpy [r4 + 8], r0
	vp_lea [r0 + view_dirty_region], r2
	f_bind gui_gui, statics, r1
	vp_lea [r1 + gui_statics_rect_heap], r0
	vp_add gui_statics_old_region, r1
	f_call gui_region, paste_region, {r0, r1, r2, 0, 0}

	;free old region and splice over new region
	f_bind gui_gui, statics, r5
	f_call gui_region, free, {&[r5 + gui_statics_rect_heap], &[r5 + gui_statics_old_region]}
	vp_pop r1
	vp_cpy r1, [r5 + gui_statics_old_region]
	vp_pop r0
%endif

	;iterate through views front to back
	;distribute visible region
	vp_cpy_cl 0, [r4 + local_ctx_flist]
	f_call view, forward_tree, {r0, r4, $distribute_down_callback, $distribute_up_callback}

	;draw all on draw list, and free dirty regions
	f_bind gui_gui, statics, r1
	vp_cpy [r1 + gui_statics_renderer], r1
	vp_cpy r1, [r4 + local_ctx + gui_ctx_sdl_ctx]
	loop_flist_forward r4 + local_ctx_flist, r0, r0
		vp_cpy r0, [r4 + local_ctx_next]
		vp_sub view_ctx_node, r0
		vp_cpy [r0 + view_ctx_x], r8
		vp_cpy [r0 + view_ctx_y], r9
		vp_lea [r4 + local_ctx], r1
		vp_lea [r0 + view_dirty_region], r2
		vp_cpy r8, [r1 + gui_ctx_x]
		vp_cpy r9, [r1 + gui_ctx_y]
		vp_cpy r2, [r1 + gui_ctx_dirty_region]
		v_call view, draw, {r0, r1}
		vp_cpy [r4 + local_ctx_next], r1
		vp_sub view_ctx_node - view_dirty_region, r1
		f_bind gui_gui, statics, r0
		vp_add gui_statics_rect_heap, r0
		f_call gui_region, free, {r0, r1}
		vp_cpy [r4 + local_ctx_next], r0
	loop_end

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
	vp_ret

abs_up_callback:
	vp_sub [r0 + view_x], r8
	vp_sub [r0 + view_y], r9
	vp_ret

visible_up_callback:
	def_structure vis
		ptr vis_inst
		ptr vis_root
		ptr vis_next
		ptr vis_region
	def_structure_end

	;save inputs
	vp_sub vis_size, r4
	vp_cpy r0, [r4 + vis_inst]
	vp_cpy r1, [r4 + vis_root]

	;region heap
	f_bind gui_gui, statics, r0
	vp_add gui_statics_rect_heap, r0

	;remove opaque region from ancestors if not root
	vp_cpy [r4 + vis_inst], r1
	if r1, !=, [r4 + vis_root]
		;remove my opaque region from ancestors
		vp_cpy_cl 0, [r4 + vis_region]

		;first copy any opaque region
		vp_cpy [r1 + view_x], r8
		vp_cpy [r1 + view_y], r9
		vp_cpy [r1 + view_parent], r2
		vp_cpy [r2 + view_w], r10
		vp_cpy [r2 + view_h], r11
		vp_mul -1, r8
		vp_mul -1, r9
		vp_add r8, r10
		vp_add r9, r11
		vp_add view_opaque_region, r1
		vp_lea [r4 + vis_region], r2
		f_call gui_region, copy_rect, {r0, r1, r2, r8, r9, r10, r11}

		;remove from ancestors
		vp_cpy [r4 + vis_inst], r1
		loop_start
			vp_cpy [r1 + view_parent], r2
			vp_cpy r2, [r4 + vis_next]

			;exit if clipped away
			vp_cpy [r4 + vis_region], r3
			breakif r3, ==, 0

			;translate temp opaque region
			vp_cpy [r1 + view_x], r8
			vp_cpy [r1 + view_y], r9
			vp_lea [r4 + vis_region], r1
			f_call gui_region, translate, {r1, r8, r9}

			;clip temp opaque region
			vp_cpy [r4 + vis_next], r2
			vp_lea [r4 + vis_region], r1
			f_call gui_region, clip_rect, {r0, r1, 0, 0, [r2 + view_w], [r2 + view_h]}

			;remove temp opaque region
			vp_lea [r4 + vis_region], r1
			vp_cpy [r4 + vis_next], r2
			vp_add view_dirty_region, r2
			f_call gui_region, remove_region, {r0, r1, r2, 0, 0}

			vp_cpy [r4 + vis_next], r1
		loop_until r1, ==, [r4 + vis_root]

		;free any temp region
		vp_lea [r4 + vis_region], r1
		f_call gui_region, free, {r0, r1}
	endif

	;clip local dirty region with parent bounds if not root
	vp_cpy [r4 + vis_inst], r1
	vp_cpy [r1 + view_parent], r2
	if r1, ==, [r4 + vis_root]
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
	f_call gui_region, clip_rect, {r0, r1, r8, r9, r10, r11}

	;paste local dirty region onto parent if not root
	vp_cpy [r4 + vis_inst], r1
	if r1, !=, [r4 + vis_root]
		vp_cpy [r1 + view_x], r8
		vp_cpy [r1 + view_y], r9
		vp_cpy [r1 + view_parent], r2
		vp_add view_dirty_region, r1
		vp_add view_dirty_region, r2
		f_call gui_region, paste_region, {r0, r1, r2, r8, r9}

		;free local dirty region
		vp_cpy [r4 + vis_inst], r1
		vp_add view_dirty_region, r1
		f_call gui_region, free, {r0, r1}
	endif

	vp_cpy [r4 + vis_inst], r0
	vp_add vis_size, r4
	vp_ret

distribute_down_callback:
	def_structure dist
		ptr dist_inst
		ptr dist_data
		ptr dist_next
	def_structure_end

	;save inputs
	vp_sub dist_size, r4
	vp_cpy r0, [r4 + dist_inst]
	vp_cpy r1, [r4 + dist_data]

	;region heap
	f_bind gui_gui, statics, r0
	vp_add gui_statics_rect_heap, r0

	;copy view from parent if not root
	vp_cpy [r4 + dist_inst], r2
	if r2, !=, [r1 + local_root]
		;copy my area from parent
		vp_cpy [r2 + view_parent], r1
		vp_cpy [r2 + view_ctx_x], r8
		vp_cpy [r2 + view_ctx_y], r9
		vp_cpy [r2 + view_w], r10
		vp_cpy [r2 + view_h], r11
		vp_add r8, r10
		vp_add r9, r11
		vp_add view_dirty_region, r1
		vp_add view_dirty_region, r2
		f_call gui_region, copy_rect, {r0, r1, r2, r8, r9, r10, r11}

		;did we find any
		vp_cpy [r4 + dist_inst], r1
		vp_cpy [r1 + view_dirty_region], r1
		if r1, !=, 0
			;remove my opaque region from ancestors
			vp_cpy [r4 + dist_inst], r2
			loop_start
				vp_cpy [r2 + view_parent], r2
				vp_cpy r2, [r4 + dist_next]

				vp_cpy [r4 + dist_inst], r1
				vp_cpy [r1 + view_ctx_x], r8
				vp_cpy [r1 + view_ctx_y], r9
				vp_add view_opaque_region, r1
				vp_add view_dirty_region, r2
				f_call gui_region, remove_region, {r0, r1, r2, r8, r9}

				vp_cpy [r4 + dist_next], r2
				vp_cpy [r4 + dist_data], r1
			loop_until r2, ==, [r1 + local_root]
		endif
	endif

	;r1 will be 0 or not depending on haveing any dirty region
	vp_cpy [r4 + dist_inst], r0
	vp_add dist_size, r4
	vp_ret

distribute_up_callback:
	;add myself to draw list if not empty
	vp_cpy [r0 + view_dirty_region], r2
	if r2, !=, 0
		vp_lea [r0 + view_ctx_node], r2
		vp_add local_ctx_flist, r1
		ln_add_fnode r1, r2, r3
	endif
	vp_ret

def_func_end
