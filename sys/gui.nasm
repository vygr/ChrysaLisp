%include "func.inc"
%include "mail.inc"
%include "gui.inc"

	fn_function "sys/gui"

		;allocate background view
		fn_call sys/gui_alloc_view

		;fill in sizes etc
		vp_cpy 0, qword[r0 + GUI_VIEW_X]
		vp_cpy 0, qword[r0 + GUI_VIEW_Y]
		vp_cpy 1024, qword[r0 + GUI_VIEW_W]
		vp_cpy 768, qword[r0 + GUI_VIEW_H]
		vp_lea [rel draw], r1
		vp_cpy r1, [r0 + GUI_VIEW_DRAW]

		;add as gui screen view
		fn_bind sys/gui_statics, r1
		vp_cpy r0, [r1 + GUI_STATICS_SCREEN]

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

	draw:
		;inputs
		;r0 = view object
		;r1 = ctx
		
		vp_ret

	fn_function_end
