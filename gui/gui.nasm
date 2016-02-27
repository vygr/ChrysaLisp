%include "func.inc"
%include "mail.inc"
%include "gui.inc"

	fn_function "gui/gui"
		;allocate background view
		fn_call gui/view_alloc

		;fill in sizes etc
		vp_cpy 0, qword[r0 + GUI_VIEW_X]
		vp_cpy 0, qword[r0 + GUI_VIEW_Y]
		vp_cpy 1024, qword[r0 + GUI_VIEW_W]
		vp_cpy 768, qword[r0 + GUI_VIEW_H]
		vp_lea [rel draw_background], r1
		vp_cpy r1, [r0 + GUI_VIEW_DRAW]

		;add as gui screen view
		fn_bind gui/gui_statics, r1
		vp_cpy r0, [r1 + GUI_STATICS_SCREEN]
		vp_push r0

		;mark for update
		fn_call gui/view_update

		;allocate sub view
		fn_call gui/view_alloc

		;fill in sizes etc
		vp_cpy 128, qword[r0 + GUI_VIEW_X]
		vp_cpy 128, qword[r0 + GUI_VIEW_Y]
		vp_cpy 512, qword[r0 + GUI_VIEW_W]
		vp_cpy 256, qword[r0 + GUI_VIEW_H]
		vp_lea [rel draw_view], r1
		vp_cpy r1, [r0 + GUI_VIEW_DRAW]

		;add as sub view
		vp_pop r1
		fn_call gui/view_add

		;mark for update
		fn_call gui/view_update

		;allocate mail message
		fn_call sys/mail_alloc

		;fill in destination, function
		vp_cpy 0, qword[r0 + ML_MSG_DEST]
		vp_cpy 0, qword[r0 + (ML_MSG_DEST + 8)]
		vp_cpy KN_CALL_GUI_UPDATE, qword[r0 + (ML_MSG_DATA + KN_DATA_KERNEL_FUNCTION)]

		;send mail to kernel
		fn_call sys/mail_send

		;wait 5 seconds and return
		vp_cpy 5000000, r0
		fn_jmp sys/task_sleep

	draw_background:
		;inputs
		;r0 = view object
		;r1 = ctx
		;trashes
		;r0-r3, r5-r15

		struc DRAW_BACKGROUND
			DRAW_BACKGROUND_VIEW:	resq 1
			DRAW_BACKGROUND_CTX:	resq 1
			DRAW_BACKGROUND_SIZE:
		endstruc

		vp_sub DRAW_BACKGROUND_SIZE, r4
		vp_cpy r0, [r4 + DRAW_BACKGROUND_VIEW]
		vp_cpy r1, [r4 + DRAW_BACKGROUND_CTX]

		vp_cpy 255, r8
		vp_cpy 0, r9
		vp_cpy 0, r10
		vp_cpy 255, r11
		vp_cpy r1, r0
		fn_call gui/ctx_set_color

		vp_cpy [r4 + DRAW_BACKGROUND_VIEW], r1
		vp_cpy [r4 + DRAW_BACKGROUND_CTX], r0
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r1 + GUI_VIEW_W], r10
		vp_cpy [r1 + GUI_VIEW_H], r11
		fn_call gui/ctx_filled_box

		vp_add DRAW_BACKGROUND_SIZE, r4
		vp_ret

	draw_view:
		;inputs
		;r0 = view object
		;r1 = ctx
		;trashes
		;r0-r3, r5-r15

		struc DRAW_VIEW
			DRAW_VIEW_VIEW:	resq 1
			DRAW_VIEW_CTX:	resq 1
			DRAW_VIEW_SIZE:
		endstruc

		vp_sub DRAW_VIEW_SIZE, r4
		vp_cpy r0, [r4 + DRAW_VIEW_VIEW]
		vp_cpy r1, [r4 + DRAW_VIEW_CTX]

		vp_cpy 255, r8
		vp_cpy 255, r9
		vp_cpy 0, r10
		vp_cpy 255, r11
		vp_cpy r1, r0
		fn_call gui/ctx_set_color

		vp_cpy [r4 + DRAW_VIEW_VIEW], r1
		vp_cpy [r4 + DRAW_VIEW_CTX], r0
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r1 + GUI_VIEW_W], r10
		vp_cpy [r1 + GUI_VIEW_H], r11
		fn_call gui/ctx_filled_box

		vp_add DRAW_VIEW_SIZE, r4
		vp_ret

	fn_function_end
