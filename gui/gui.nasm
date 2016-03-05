%include 'inc/func.inc'
%include 'inc/task.inc'
%include 'inc/gui.inc'

	fn_function gui/gui
		;allocate background view
		fn_call gui/view_alloc

		;fill in sizes etc
		vp_cpy 0, qword[r0 + gui_view_x]
		vp_cpy 0, qword[r0 + gui_view_y]
		vp_cpy 1024, qword[r0 + gui_view_w]
		vp_cpy 768, qword[r0 + gui_view_h]
		vp_lea [rel draw_background_code], r1
		vp_cpy r1, [r0 + gui_view_draw]

		;add as gui screen view
		fn_bind gui/gui_statics, r1
		vp_cpy r0, [r1 + gui_statics_screen]
		vp_push r0

		;allocate sub view
		fn_call gui/view_alloc

		;fill in sizes etc
		vp_cpy 128, qword[r0 + gui_view_x]
		vp_cpy 128, qword[r0 + gui_view_y]
		vp_cpy 512, qword[r0 + gui_view_w]
		vp_cpy 256, qword[r0 + gui_view_h]
		vp_lea [rel draw_view1_code], r1
		vp_cpy r1, [r0 + gui_view_draw]
		vp_push r0

		;allocate sub view
		fn_call gui/view_alloc

		;fill in sizes etc
		vp_cpy 256, qword[r0 + gui_view_x]
		vp_cpy 256, qword[r0 + gui_view_y]
		vp_cpy 512, qword[r0 + gui_view_w]
		vp_cpy 256, qword[r0 + gui_view_h]
		vp_lea [rel draw_view2_code], r1
		vp_cpy r1, [r0 + gui_view_draw]
		vp_push r0

		;add as sub view
		vp_cpy [r4 + 16], r1
		vp_cpy [r4 + 8], r0
		fn_call gui/view_add

		;add as sub view
		vp_cpy [r4 + 16], r1
		vp_cpy [r4], r0
		fn_call gui/view_add

		;mark for update and as transparent for cyan view
		vp_cpy [r4], r0
		fn_call gui/view_transparent
		vp_pop r0
		fn_call gui/view_dirty

		;mark for update for yellow view
		vp_pop r0
		fn_call gui/view_dirty

		;mark for update for background
		vp_pop r0
		fn_call gui/view_dirty

		;allocate mail message
		class_call mail, alloc
		fn_assert r0, !=, 0

		;fill in destination, function
		vp_cpy 0, qword[r0 + ml_msg_dest]
		vp_cpy 0, qword[r0 + (ml_msg_dest + 8)]
		vp_cpy kn_call_gui_update, qword[r0 + (ml_msg_data + kn_data_kernel_function)]

		;send mail to kernel
		class_call mail, send

		;wait 5 seconds and return
		vp_cpy 5000000, r0
		class_jmp task, sleep

	draw_background_code:
		;inputs
		;r0 = view object
		;r1 = ctx
		;trashes
		;r0-r3, r5-r15

		struc draw_background
			draw_background_view:	resq 1
			draw_background_ctx:	resq 1
		endstruc

		vp_sub draw_background_size, r4
		vp_cpy r0, [r4 + draw_background_view]
		vp_cpy r1, [r4 + draw_background_ctx]

		vp_cpy 0, r8
		vp_cpy 0, r9
		vp_cpy 0, r10
		vp_cpy 255, r11
		vp_cpy r1, r0
		fn_call gui/ctx_set_color

		vp_cpy [r4 + draw_background_view], r1
		vp_cpy [r4 + draw_background_ctx], r0
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r1 + gui_view_w], r10
		vp_cpy [r1 + gui_view_h], r11
		fn_call gui/ctx_filled_box

		vp_add draw_background_size, r4
		vp_ret

	draw_view1_code:
		;inputs
		;r0 = view object
		;r1 = ctx
		;trashes
		;r0-r3, r5-r15

		struc draw_view1
			draw_view1_view:	resq 1
			draw_view1_ctx:		resq 1
		endstruc

		vp_sub draw_view1_size, r4
		vp_cpy r0, [r4 + draw_view1_view]
		vp_cpy r1, [r4 + draw_view1_ctx]

		vp_cpy 255, r8
		vp_cpy 255, r9
		vp_cpy 0, r10
		vp_cpy 255, r11
		vp_cpy r1, r0
		fn_call gui/ctx_set_color

		vp_cpy [r4 + draw_view1_view], r1
		vp_cpy [r4 + draw_view1_ctx], r0
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r1 + gui_view_w], r10
		vp_cpy [r1 + gui_view_h], r11
		fn_call gui/ctx_filled_box

		vp_add draw_view1_size, r4
		vp_ret

	draw_view2_code:
		;inputs
		;r0 = view object
		;r1 = ctx
		;trashes
		;r0-r3, r5-r15

		struc draw_view2
			draw_view2_view:	resq 1
			draw_view2_ctx:		resq 1
		endstruc

		vp_sub draw_view2_size, r4
		vp_cpy r0, [r4 + draw_view2_view]
		vp_cpy r1, [r4 + draw_view2_ctx]

		vp_cpy 0, r8
		vp_cpy 255, r9
		vp_cpy 255, r10
		vp_cpy 192, r11
		vp_cpy r1, r0
		fn_call gui/ctx_set_color

		vp_cpy [r4 + draw_view2_view], r1
		vp_cpy [r4 + draw_view2_ctx], r0
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r1 + gui_view_w], r10
		vp_cpy [r1 + gui_view_h], r11
		fn_call gui/ctx_filled_box

		vp_add draw_view2_size, r4
		vp_ret

	fn_function_end
