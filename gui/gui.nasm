%include 'inc/func.inc'
%include 'inc/task.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

%define SCREEN_WIDTH 1024
%define SCREEN_HEIGHT 768

	fn_function gui/gui
		;allocate background view
		static_call view, create

		;fill in sizes etc
		vp_cpy 0, qword[r0 + view_x]
		vp_cpy 0, qword[r0 + view_y]
		vp_cpy SCREEN_WIDTH, qword[r0 + view_w]
		vp_cpy SCREEN_HEIGHT, qword[r0 + view_h]
		vp_cpy 0, qword[r0 + view_red]
		vp_cpy 0, qword[r0 + view_green]
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 255, qword[r0 + view_alpha]

		;add as gui screen view
		static_bind gui, statics, r1
		vp_cpy r0, [r1 + gui_statics_screen]
		vp_push r0
		static_call view, opaque

		;allocate sub view
		static_call view, create

		;fill in sizes etc
		vp_cpy 128, qword[r0 + view_x]
		vp_cpy 128, qword[r0 + view_y]
		vp_cpy 512, qword[r0 + view_w]
		vp_cpy 256, qword[r0 + view_h]
		vp_cpy 255, qword[r0 + view_red]
		vp_cpy 0, qword[r0 + view_green]
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
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 128, qword[r0 + view_alpha]
		vp_push r0

		;add as sub view
		vp_cpy [r4 + 16], r1
		vp_cpy [r4 + 8], r0
		static_call view, add

		;add as sub view
		vp_cpy [r4 + 16], r1
		vp_cpy [r4], r0
		static_call view, add

		;opaque for red view
		vp_cpy [r4 + 8], r0
		static_call view, opaque

		vp_add 3*8, r4

		;dirty all
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_screen], r0
		static_call view, dirty_all

		for r15, 0, 2, 1
			vp_push r15
				;allocate mail message
				static_call mail, alloc
				fn_assert r0, !=, 0

				;fill in destination, function
				vp_cpy 0, qword[r0 + ml_msg_dest]
				vp_cpy 0, qword[r0 + (ml_msg_dest + 8)]
				vp_cpy kn_call_gui_update, qword[r0 + (ml_msg_data + kn_data_kernel_function)]

				;send mail to kernel
				static_call mail, send

				;wait till window alive
				vp_cpy 2000000, r0
				static_call task, sleep
			vp_pop r15
		next

		;gui event loop
		for r15, 0, 1000, 1
			vp_push r15

			;allocate mail message
			static_call mail, alloc
			fn_assert r0, !=, 0

			;fill in destination, function
			vp_cpy 0, qword[r0 + ml_msg_dest]
			vp_cpy 0, qword[r0 + (ml_msg_dest + 8)]
			vp_cpy kn_call_gui_update, qword[r0 + (ml_msg_data + kn_data_kernel_function)]

			;send mail to kernel
			static_call mail, send

			;yield to other tasks
			vp_cpy 1000000 / 60, r0
			static_call task, sleep

			;get mouse info
			static_bind gui, statics, r5
			vp_cpy [r5 + gui_statics_x_pos], r8
			vp_cpy [r5 + gui_statics_y_pos], r9
			vp_cpy [r5 + gui_statics_buttons], r10
			vp_cpy [r5 + gui_statics_screen], r0
			static_call view, hit_tree
			vp_cpy 1, r1
			if r0, ==, [r5 + gui_statics_screen]
				vp_xor r1, r1
			endif
			if r0, ==, 0
				vp_xor r1, r1
			endif
			switch
			case r1, ==, 0
				;no hit
			case r10, ==, 0
				;no buttons
				vp_xor r1, r1
				vp_cpy r1, [r5 + gui_statics_last_view]
				break
			case qword[r5 + gui_statics_last_view], !=, 0
				;move
				vp_cpy [r5 + gui_statics_x_pos], r8
				vp_cpy [r5 + gui_statics_y_pos], r9
				vp_sub [r5 + gui_statics_last_x_pos], r8
				vp_sub [r5 + gui_statics_last_y_pos], r9
				vp_cpy [r5 + gui_statics_last_view], r0
				vp_cpy [r0 + view_w], r10
				vp_cpy [r0 + view_h], r11
				static_call view, move
				break
			default
				;down
				vp_cpy r0, [r5 + gui_statics_last_view]
				vp_cpy r8, [r5 + gui_statics_last_x_pos]
				vp_cpy r9, [r5 + gui_statics_last_y_pos]
			endswitch

			vp_pop r15
		next
		vp_ret

	fn_function_end
