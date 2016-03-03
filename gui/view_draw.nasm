%include 'inc/func.inc'
%include 'inc/list.inc'
%include 'inc/gui.inc'

	struc DRAW_VIEW
		DRAW_VIEW_CTX:			resb GUI_CTX_SIZE
		DRAW_VIEW_ROOT:			resq 1
		DRAW_VIEW_NODE:			resq 1
		DRAW_VIEW_PATCH_LIST:	resq 1
		DRAW_VIEW_SIZE:
	endstruc

	fn_function gui/view_draw
		;inputs
		;r0 = view object
		;trashes
		;r0-r3, r5-r15

		vp_sub DRAW_VIEW_SIZE, r4
		vp_cpy r0, [r4 + DRAW_VIEW_ROOT]
		fn_bind gui/gui_statics, r1
		vp_cpy [r1 + GUI_STATICS_RENDERER], r1
		vp_cpy r1, [r4 + DRAW_VIEW_CTX + GUI_CTX_SDL_CTX]
		vp_cpy 0, qword[r4 + DRAW_VIEW_PATCH_LIST]

		;iterate through views back to front
		;setting abs cords
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r4 + DRAW_VIEW_ROOT], r1
		loop_start
		down_loop_ctx:
			vp_cpy r1, r0

			;context abs cords
			vp_add [r0 + GUI_VIEW_X], r8
			vp_add [r0 + GUI_VIEW_Y], r9
			vp_cpy r8, [r0 + GUI_VIEW_CTX_X]
			vp_cpy r9, [r0 + GUI_VIEW_CTX_Y]

			;down to child
			lh_get_head r0 + GUI_VIEW_LIST, r1
			vp_sub GUI_VIEW_NODE, r1
		loop_until qword[r1 + GUI_VIEW_NODE + ln_node_succ], ==, 0
		loop_while r0, !=, [r4 + DRAW_VIEW_ROOT]
			;context abs cords
			vp_sub [r0 + GUI_VIEW_X], r8
			vp_sub [r0 + GUI_VIEW_Y], r9

			;across to sibling
			ln_get_succ r0 + GUI_VIEW_NODE, r1
			vp_sub GUI_VIEW_NODE, r1
			jmpif qword[r1 + GUI_VIEW_NODE + ln_node_succ], !=, 0, down_loop_ctx

			;up to parent
			vp_cpy [r0 + GUI_VIEW_PARENT], r0
		loop_end

		;iterate through views back to front
		;create visible patch list
		vp_cpy [r4 + DRAW_VIEW_ROOT], r1
		loop_start
		down_loop_back_to_front:
			vp_cpy r1, r0

			;save node
			vp_cpy r0, [r4 + DRAW_VIEW_NODE]

			;patch heap
			fn_bind gui/gui_statics, r0
			vp_lea [r0 + GUI_STATICS_PATCH_HEAP], r0

			;if opaque view remove from global dirty list
			vp_cpy [r4 + DRAW_VIEW_NODE], r1
			vp_cpy [r1 + GUI_VIEW_TRANSPARENT_LIST], r2
			if r2, ==, 0
				vp_cpy [r1 + GUI_VIEW_CTX_X], r8
				vp_cpy [r1 + GUI_VIEW_CTX_Y], r9
				vp_cpy [r1 + GUI_VIEW_W], r10
				vp_cpy [r1 + GUI_VIEW_H], r11
				vp_add r8, r10
				vp_add r9, r11
				vp_lea [r4 + DRAW_VIEW_PATCH_LIST], r1
				fn_call gui/patch_remove
			endif

			;clip local dirty list with parent bounds
			vp_cpy [r4 + DRAW_VIEW_NODE], r1
			vp_cpy [r1 + GUI_VIEW_PARENT], r2
			if r2, ==, 0
				vp_cpy r1, r2
			endif
			vp_cpy [r2 + GUI_VIEW_CTX_X], r8
			vp_cpy [r2 + GUI_VIEW_CTX_Y], r9
			vp_cpy [r2 + GUI_VIEW_W], r10
			vp_cpy [r2 + GUI_VIEW_H], r11
			vp_add r8, r10
			vp_add r9, r11
			vp_add GUI_VIEW_DIRTY_LIST, r1
			fn_call gui/patch_clip

			;paste local dirty list onto global dirty list
			vp_lea [r4 + DRAW_VIEW_PATCH_LIST], r2
			fn_call gui/patch_list_paste

			;free local dirty list
			vp_cpy [r4 + DRAW_VIEW_NODE], r1
			vp_lea [r1 + GUI_VIEW_DIRTY_LIST], r1
			fn_call gui/patch_list_free

			;restore node
			vp_cpy [r4 + DRAW_VIEW_NODE], r0

			;down to child
			lh_get_head r0 + GUI_VIEW_LIST, r1
			vp_sub GUI_VIEW_NODE, r1
		loop_until qword[r1 + GUI_VIEW_NODE + ln_node_succ], ==, 0
		loop_while r0, !=, [r4 + DRAW_VIEW_ROOT]
			;across to sibling
			ln_get_succ r0 + GUI_VIEW_NODE, r1
			vp_sub GUI_VIEW_NODE, r1
			jmpif qword[r1 + GUI_VIEW_NODE + ln_node_succ], !=, 0, down_loop_back_to_front

			;up to parent
			vp_cpy [r0 + GUI_VIEW_PARENT], r0
		loop_end

		;iterate through views front to back
		;distribute visible patch list
		vp_cpy [r4 + DRAW_VIEW_ROOT], r1
		loop_start
		down_loop_front_to_back:
			vp_cpy r1, r0

			;down to child
			lh_get_tail r0 + GUI_VIEW_LIST, r1
			vp_sub GUI_VIEW_NODE, r1
		loop_until qword[r1 + GUI_VIEW_NODE + ln_node_pred], ==, 0
		loop_while r0, !=, [r4 + DRAW_VIEW_ROOT]
			;save node
			vp_cpy r0, [r4 + DRAW_VIEW_NODE]

			;if opaque cut view else copy transparent patches
			vp_cpy [r0 + GUI_VIEW_TRANSPARENT_LIST], r1
			if r1, ==, 0
				vp_cpy [r0 + GUI_VIEW_CTX_X], r8
				vp_cpy [r0 + GUI_VIEW_CTX_Y], r9
				vp_cpy [r0 + GUI_VIEW_W], r10
				vp_cpy [r0 + GUI_VIEW_H], r11
				vp_add r8, r10
				vp_add r9, r11
				vp_lea [r4 + DRAW_VIEW_PATCH_LIST], r1
				vp_lea [r0 + GUI_VIEW_DIRTY_LIST], r2
				fn_bind gui/gui_statics, r0
				vp_lea [r0 + GUI_STATICS_PATCH_HEAP], r0
				fn_call gui/patch_cut
			else
				vp_lea [r4 + DRAW_VIEW_PATCH_LIST], r1
				vp_lea [r0 + GUI_VIEW_DIRTY_LIST], r2
				vp_lea [r0 + GUI_VIEW_TRANSPARENT_LIST], r3
				fn_bind gui/gui_statics, r0
				vp_lea [r0 + GUI_STATICS_PATCH_HEAP], r0
				fn_call gui/patch_list_copy
			endif

			;restore node
			vp_cpy [r4 + DRAW_VIEW_NODE], r0

			;across to sibling
			ln_get_pred r0 + GUI_VIEW_NODE, r1
			vp_sub GUI_VIEW_NODE, r1
			jmpif qword[r1 + GUI_VIEW_NODE + ln_node_pred], !=, 0, down_loop_front_to_back

			;up to parent
			vp_cpy [r0 + GUI_VIEW_PARENT], r0
		loop_end

		;any remaining patches are root views
		vp_cpy [r4 + DRAW_VIEW_ROOT], r1
		vp_cpy [r4 + DRAW_VIEW_PATCH_LIST], r0
		vp_cpy r0, [r1 + GUI_VIEW_DIRTY_LIST]
		vp_cpy 0, qword[r4 + DRAW_VIEW_PATCH_LIST]

		;iterate through views back to front drawing
		vp_cpy [r4 + DRAW_VIEW_ROOT], r1
		loop_start
		down_loop_draw:
			vp_cpy r1, r0

			;save node
			vp_cpy r0, [r4 + DRAW_VIEW_NODE]

			;draw myself
			vp_lea [r4 + DRAW_VIEW_CTX], r1
			vp_lea [r0 + GUI_VIEW_DIRTY_LIST], r2
			vp_cpy [r0 + GUI_VIEW_CTX_X], r8
			vp_cpy [r0 + GUI_VIEW_CTX_Y], r9
			vp_cpy r2, [r1 + GUI_CTX_DIRTY_REGION]
			vp_cpy r8, [r1 + GUI_CTX_X]
			vp_cpy r9, [r1 + GUI_CTX_Y]
			vp_call [r0 + GUI_VIEW_DRAW]

			;free local dirty list
			vp_cpy [r4 + DRAW_VIEW_NODE], r1
			vp_lea [r1 + GUI_VIEW_DIRTY_LIST], r1
			fn_bind gui/gui_statics, r0
			vp_lea [r0 + GUI_STATICS_PATCH_HEAP], r0
			fn_call gui/patch_list_free

			;restore node
			vp_cpy [r4 + DRAW_VIEW_NODE], r0

			;down to child
			lh_get_head r0 + GUI_VIEW_LIST, r1
			vp_sub GUI_VIEW_NODE, r1
		loop_until qword[r1 + GUI_VIEW_NODE + ln_node_succ], ==, 0
		loop_while r0, !=, [r4 + DRAW_VIEW_ROOT]
			;across to sibling
			ln_get_succ r0 + GUI_VIEW_NODE, r1
			vp_sub GUI_VIEW_NODE, r1
			jmpif qword[r1 + GUI_VIEW_NODE + ln_node_succ], !=, 0, down_loop_draw

			;up to parent
			vp_cpy [r0 + GUI_VIEW_PARENT], r0
		loop_end

		vp_add DRAW_VIEW_SIZE, r4
		vp_ret

	fn_function_end
