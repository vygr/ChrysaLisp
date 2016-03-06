%include 'inc/func.inc'
%include 'inc/task.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function gui/gui
		;allocate background view
		static_call view, create

		;fill in sizes etc
		vp_cpy 0, qword[r0 + view_x]
		vp_cpy 0, qword[r0 + view_y]
		vp_cpy 1024, qword[r0 + view_w]
		vp_cpy 768, qword[r0 + view_h]
		vp_cpy 0, qword[r0 + view_red]
		vp_cpy 0, qword[r0 + view_green]
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 255, qword[r0 + view_alpha]

		;add as gui screen view
		static_bind gui, statics, r1
		vp_cpy r0, [r1 + gui_statics_screen]
		vp_push r0

		;allocate sub view
		static_call view, create

		;fill in sizes etc
		vp_cpy 128, qword[r0 + view_x]
		vp_cpy 128, qword[r0 + view_y]
		vp_cpy 512, qword[r0 + view_w]
		vp_cpy 256, qword[r0 + view_h]
		vp_cpy 255, qword[r0 + view_red]
		vp_cpy 255, qword[r0 + view_green]
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 255, qword[r0 + view_alpha]
		vp_push r0

		;allocate sub view
		static_call view, create

		;fill in sizes etc
		vp_cpy 256, qword[r0 + view_x]
		vp_cpy 256, qword[r0 + view_y]
		vp_cpy 512, qword[r0 + view_w]
		vp_cpy 256, qword[r0 + view_h]
		vp_cpy 0, qword[r0 + view_red]
		vp_cpy 255, qword[r0 + view_green]
		vp_cpy 255, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_push r0

		;add as sub view
		vp_cpy [r4 + 16], r1
		vp_cpy [r4 + 8], r0
		static_call view, add

		;add as sub view
		vp_cpy [r4 + 16], r1
		vp_cpy [r4], r0
		static_call view, add

		;mark for update and as transparent for cyan view
		vp_cpy [r4], r0
		static_call view, transparent
		vp_pop r0
		static_call view, dirty

		;mark for update for yellow view
		vp_pop r0
		static_call view, dirty

		;mark for update for background
		vp_pop r0
		static_call view, dirty

		;allocate mail message
		static_call mail, alloc
		fn_assert r0, !=, 0

		;fill in destination, function
		vp_cpy 0, qword[r0 + ml_msg_dest]
		vp_cpy 0, qword[r0 + (ml_msg_dest + 8)]
		vp_cpy kn_call_gui_update, qword[r0 + (ml_msg_data + kn_data_kernel_function)]

		;send mail to kernel
		static_call mail, send

		;wait 5 seconds and return
		vp_cpy 5000000, r0
		static_jmp task, sleep

	fn_function_end
