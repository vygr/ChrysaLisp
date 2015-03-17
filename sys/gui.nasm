%include "func.inc"
%include "mail.inc"
%include "gui.inc"
%include "sdl2.inc"
%include "syscall.inc"

	fn_function "sys/gui"

		;allocate background view
		fn_call sys/gui_alloc_view

		;fill in sizes etc
		vp_cpy 0, qword[r0 + GUI_VIEW_X]
		vp_cpy 0, qword[r0 + GUI_VIEW_Y]
		vp_cpy 1024, qword[r0 + GUI_VIEW_W]
		vp_cpy 768, qword[r0 + GUI_VIEW_H]
		vp_lea [rel draw_background], r1
		vp_cpy r1, [r0 + GUI_VIEW_DRAW]

		;add as gui screen view
		fn_bind sys/gui_statics, r1
		vp_cpy r0, [r1 + GUI_STATICS_SCREEN]
		vp_push r0

		;allocate sub view
		fn_call sys/gui_alloc_view

		;fill in sizes etc
		vp_cpy 128, qword[r0 + GUI_VIEW_X]
		vp_cpy 128, qword[r0 + GUI_VIEW_Y]
		vp_cpy 512, qword[r0 + GUI_VIEW_W]
		vp_cpy 256, qword[r0 + GUI_VIEW_H]
		vp_lea [rel draw_view], r1
		vp_cpy r1, [r0 + GUI_VIEW_DRAW]

		;add as sub view
		vp_pop r1
		fn_call sys/gui_add_view

		;allocate mail message
		fn_call sys/mail_alloc

		;fill in destination, function
		vp_cpy 0, qword[r0 + ML_MSG_DEST]
		vp_cpy 0, qword[r0 + (ML_MSG_DEST + 8)]
		vp_cpy KN_CALL_GUI_UPDATE, qword[r0 + (ML_MSG_DATA + KN_DATA_KERNEL_FUNCTION)]

		;send mail to kernel
		fn_call sys/mail_send

		;wait 5 seconds
		vp_cpy 5000000, r0
		fn_call sys/task_sleep

		;stop this task
		fn_jmp sys/task_stop

	draw_background:
		;inputs
		;r0 = view object
		;r1 = renderer
		;trashes
		;r0-r3, r5-r14

		vp_push r1
		vp_sub SDL_RECT_SIZE, r4
		vp_cpy [r0 + GUI_VIEW_X], r8
		vp_cpy [r0 + GUI_VIEW_Y], r9
		vp_cpy [r0 + GUI_VIEW_W], r10
		vp_cpy [r0 + GUI_VIEW_H], r11
		vp_cpy r8d, [r4 + SDL_RECT_X]
		vp_cpy r9d, [r4 + SDL_RECT_Y]
		vp_cpy r10d, [r4 + SDL_RECT_W]
		vp_cpy r11d, [r4 + SDL_RECT_H]
		sdl_setrenderdrawcolor [r4 + SDL_RECT_SIZE], 255, 0, 0, 255
		sdl_renderfillrect [r4 + SDL_RECT_SIZE], r4
		vp_add SDL_RECT_SIZE, r4
		vp_add 8, r4
		vp_ret

	draw_view:
		;inputs
		;r0 = view object
		;r1 = renderer
		;trashes
		;r0-r3, r5-r14

		vp_push r1
		vp_sub SDL_RECT_SIZE, r4
		vp_cpy [r0 + GUI_VIEW_X], r8
		vp_cpy [r0 + GUI_VIEW_Y], r9
		vp_cpy [r0 + GUI_VIEW_W], r10
		vp_cpy [r0 + GUI_VIEW_H], r11
		vp_cpy r8d, [r4 + SDL_RECT_X]
		vp_cpy r9d, [r4 + SDL_RECT_Y]
		vp_cpy r10d, [r4 + SDL_RECT_W]
		vp_cpy r11d, [r4 + SDL_RECT_H]
		sdl_setrenderdrawcolor [r4 + SDL_RECT_SIZE], 255, 255, 0, 255
		sdl_renderfillrect [r4 + SDL_RECT_SIZE], r4
		vp_add SDL_RECT_SIZE, r4
		vp_add 8, r4
		vp_ret

	fn_function_end
