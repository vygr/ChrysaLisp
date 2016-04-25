%include 'inc/func.inc'
%include 'inc/task.inc'
%include 'inc/gui.inc'
%include 'inc/sdl2.inc'
%include 'class/class_label.inc'

	fn_function gui/gui
		;kernel callback for first update
		;this will init SDL etc
		vp_rel kernel_callback, r0
		static_call sys_task, callback

		;allocate background view
		static_call label, create

		;set as gui screen view
		static_bind gui_gui, statics, r1
		vp_cpy r0, [r1 + gui_statics_screen]

		;size and color and opaque
		static_call label, change, {r0, 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT}
		static_call label, set_color, {r0, 0xff000000}
		static_call label, opaque
		static_call label, dirty_all

		;sleep just a moment to let all routeing finish
		static_call sys_task, sleep, {1000000}

		;for now fire up the test apps
		;this might be an gui auto run list eventually
		static_call sys_task, start, {@tests/gui/gui1/app}
		static_call sys_task, start, {@tests/gui/gui2/app}
		static_call sys_task, start, {@tests/gui/gui3/app}

		;gui event loop
		loop_start
		next_frame:
			;kernel callback for update
			vp_rel kernel_callback, r0
			static_call sys_task, callback

			;frame rate of gui updates
			static_call sys_task, sleep, {1000000 / 60}

			;get mouse info, see if any change
			static_bind gui_gui, statics, r5
			vp_cpy [r5 + gui_statics_x_pos], r8
			vp_cpy [r5 + gui_statics_y_pos], r9
			vp_cpy [r5 + gui_statics_buttons], r10
			if r8, ==, [r5 + gui_statics_last_x_pos]
				if r9, ==, [r5 + gui_statics_last_y_pos]
					if r10, ==, [r5 + gui_statics_last_buttons]
						;same as last time
						vp_jmp next_frame
					endif
				endif
			endif
			vp_cpy r8, [r5 + gui_statics_last_x_pos]
			vp_cpy r9, [r5 + gui_statics_last_y_pos]
			vp_cpy r10, [r5 + gui_statics_last_buttons]

			;dispatch to task and target view
			vp_cpy [r5 + gui_statics_last_view], r6
			if r6, !=, 0
			send_mouse:
				;do we need to wait till button goes up ?
				if r6, !=, -1
					;lookup view owner
					static_call view, find_owner, {r6}
					if r1, !=, 0
						;save owner mailbox
						static_call sys_cpu, id, {}, {r15}
						vp_lea [r1 + tk_node_mailbox], r14

						;allocate mail message
						static_call sys_mail, alloc
						assert r0, !=, 0

						;fill in data
						vp_cpy r14, [r0 + ml_msg_dest]
						vp_cpy r15, [r0 + (ml_msg_dest + 8)]
						vp_cpy [r5 + gui_statics_x_pos], r8
						vp_cpy [r5 + gui_statics_y_pos], r9
						vp_cpy_cl ev_type_mouse, [r0 + ev_data_type]
						vp_cpy r6, [r0 + ev_data_view]
						vp_cpy r8, [r0 + ev_data_x]
						vp_cpy r9, [r0 + ev_data_y]
						vp_cpy r10, [r0 + ev_data_buttons]
						vp_sub [r6 + view_ctx_x], r8
						vp_sub [r6 + view_ctx_y], r9
						vp_cpy r8, [r0 + ev_data_rx]
						vp_cpy r9, [r0 + ev_data_ry]

						;send mail to owner
						static_call sys_mail, send
					endif
				endif

				;if button went up then clear locked view
				if r10, ==, 0
					vp_cpy r10, [r5 + gui_statics_last_view]
				endif
			else
				;button down ?
				if r10, !=, 0
					;find view
					static_call view, hit_tree, {[r5 + gui_statics_screen], \
												[r5 + gui_statics_x_pos], \
												[r5 + gui_statics_y_pos]}
					if r1, ==, [r5 + gui_statics_screen]
						vp_xor r1, r1
					endif
					if r1, ==, 0
						vp_cpy -1, r1
					endif
					vp_cpy r1, [r5 + gui_statics_last_view]
					vp_cpy r1, r6
					vp_jmp send_mouse
				else
					;hover
				endif
			endif
		loop_end
		vp_ret

	kernel_callback:
		;inputs
		;r0 = user data

		def_structure	local
			def_long	local_old_stack
		def_structure_end

		;align stack
		vp_cpy r4, r1
		vp_sub local_size, r4
		vp_and -16, r4
		vp_cpy r1, [r4 + local_old_stack]

		;create screen window ?
		static_bind gui_gui, statics, r0
		vp_cpy [r0 + gui_statics_window], r1
		if r1, ==, 0
			;init sdl2
			sdl_set_main_ready
			sdl_init SDL_INIT_VIDEO
			ttf_init

			;create window
			vp_rel title, r0
			sdl_create_window r0, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_OPENGL
			static_bind gui_gui, statics, r1
			vp_cpy r0, [r1 + gui_statics_window]

			;create renderer
			sdl_create_renderer r0, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC
			static_bind gui_gui, statics, r1
			vp_cpy r0, [r1 + gui_statics_renderer]

			;set blend mode
			sdl_set_render_draw_blend_mode r0, SDL_BLENDMODE_BLEND
		endif

		;update screen
		static_bind gui_gui, statics, r0
		vp_cpy [r0 + gui_statics_screen], r0
		if r0, !=, 0
			;pump sdl events
			sdl_pump_events

			;get mouse state
			static_bind gui_gui, statics, r0
			vp_lea [r0 + gui_statics_x_pos], r1
			vp_lea [r0 + gui_statics_y_pos], r2
			sdl_get_mouse_state r1, r2
			static_bind gui_gui, statics, r1
			vp_add gui_statics_buttons, r1
			vp_cpy r0, [r1]

			;update the screen
			static_bind gui_gui, statics, r0
			static_call gui_gui, update, {[r0 + gui_statics_screen]}

			;refresh the window
			static_bind gui_gui, statics, r0
			sdl_render_present [r0 + gui_statics_renderer]
		endif

		vp_cpy [r4 + local_old_stack], r4
		vp_ret

	title:
		db 'Asm Kernel GUI Window', 0

	fn_function_end
