%include 'inc/func.inc'
%include 'inc/task.inc'
%include 'inc/gui.inc'
%include 'inc/sdl2.inc'
%include 'class/class_label.inc'

	def_structure local
		long local_x_pos
		long local_y_pos
		long local_buttons
		long local_last_x_pos
		long local_last_y_pos
		long local_last_buttons
		long local_last_view
		long local_keymap
		uint local_keymap_size
	def_structure_end

	fn_function gui/gui
		;init vars
		vp_sub local_size, r4
		vp_xor r0, r0
		vp_cpy r0, [r4 + local_last_x_pos]
		vp_cpy r0, [r4 + local_last_y_pos]
		vp_cpy r0, [r4 + local_last_buttons]
		vp_cpy r0, [r4 + local_last_view]
		static_bind gui_gui, statics, r1
		vp_cpy r0, [r1 + gui_statics_screen]

		;kernel callback for first update
		;this will init SDL etc
		s_call sys_task, callback, {$update_callback, r4}

		;allocate background view for screen
		s_call label, create, {}, {r0}
		static_bind gui_gui, statics, r1
		vp_cpy r0, [r1 + gui_statics_screen]

		;size and color and opaque
		s_call label, change, {r0, 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT}
		s_call label, set_color, {r0, 0xff000000}
		s_call label, opaque, {r0}
		s_call label, dirty_all, {r0}

		;sleep just a moment to let all routeing finish
		s_call sys_task, sleep, {1000000}

		;for now fire up the test apps
		;this might be an gui auto run list eventually
		s_call sys_task, start, {@tests/gui/gui2/app}, {r0, r1}

		;gui event loop
		loop_start
		next_frame:
			;kernel callback for update
			s_call sys_task, callback, {$update_callback, r4}

			;frame rate of gui updates
			s_call sys_task, sleep, {1000000 / 60}

			;get keyboard info, see if any changes
			vp_cpy [r4 + local_keymap], r0
			vp_cpy_ui [r4 + local_keymap_size], r1
			vp_cpy_ub [r0 + 0xe1], r8
			vp_cpy_ub [r0 + 0xe5], r9
			vp_or r8, r9
			loop_while r1, !=, 0
				vp_dec r1
				vp_cpy_ub [r0 + r1], r8
				if r8, !=, 0
;					fn_debug_long "Key Scancode = ", r1
					vp_rel scan_codes, r2
					vp_rel scan_codes_end, r3
					loop_start
						vp_cpy_ub [r2], r8
						if r8, ==, r1
							vp_cpy_ub [r2 + r9 + 1], r8
							fn_debug_long "Key = ", r8
						endif
						vp_add 3, r2
					loop_until r2, >=, r3
				endif
			loop_end

			;get mouse info, see if any changes
			vp_cpy [r4 + local_x_pos], r8
			vp_cpy [r4 + local_y_pos], r9
			vp_cpy [r4 + local_buttons], r10
			if r8, ==, [r4 + local_last_x_pos]
				if r9, ==, [r4 + local_last_y_pos]
					if r10, ==, [r4 + local_last_buttons]
						;same as last time
						vp_jmp next_frame
					endif
				endif
			endif
			vp_cpy r8, [r4 + local_last_x_pos]
			vp_cpy r9, [r4 + local_last_y_pos]
			vp_cpy r10, [r4 + local_last_buttons]

			;dispatch to task and target view
			vp_cpy [r4 + local_last_view], r6
			if r6, !=, 0
			send_mouse:
				;do we need to wait till button goes up ?
				if r6, !=, -1
					;lookup view owner
					s_call view, find_owner, {r6}, {r1}
					if r1, !=, 0
						;save owner mailbox
						s_call sys_cpu, id, {}, {r15}
						vp_lea [r1 + tk_node_mailbox], r14

						;allocate mail message
						s_call sys_mail, alloc, {}, {r0}
						assert r0, !=, 0

						;fill in data
						vp_cpy r14, [r0 + ml_msg_dest]
						vp_cpy r15, [r0 + (ml_msg_dest + 8)]
						vp_cpy [r4 + local_x_pos], r8
						vp_cpy [r4 + local_y_pos], r9
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
						s_call sys_mail, send, {r0}
					endif
				endif

				;if button went up then clear locked view
				if r10, ==, 0
					vp_cpy r10, [r4 + local_last_view]
				endif
			else
				;button down ?
				if r10, !=, 0
					;find view
					static_bind gui_gui, statics, r5
					s_call view, hit_tree, {[r5 + gui_statics_screen], \
												[r4 + local_x_pos], \
												[r4 + local_y_pos]}, {r1, r8, r9}
					if r1, ==, [r5 + gui_statics_screen]
						vp_xor r1, r1
					endif
					if r1, ==, 0
						vp_cpy -1, r1
					endif
					vp_cpy r1, [r4 + local_last_view]
					vp_cpy r1, r6
					vp_jmp send_mouse
				else
					;hover
				endif
			endif
		loop_end

		;deinit
		s_call sys_task, callback, {$deinit_callback, r4}

		vp_add local_size, r4
		vp_ret

	update_callback:
		;inputs
		;r0 = user data

		def_structure klocal
			long klocal_old_stack
			long klocal_user
		def_structure_end

		;align stack
		vp_cpy r4, r1
		vp_sub klocal_size, r4
		vp_and -16, r4
		vp_cpy r1, [r4 + klocal_old_stack]
		vp_cpy r0, [r4 + klocal_user]

		;create screen window ?
		static_bind gui_gui, statics, r0
		vp_cpy [r0 + gui_statics_window], r1
		if r1, ==, 0
			;init sdl2
			sdl_set_main_ready
			sdl_init SDL_INIT_VIDEO
			ttf_init

			;create window
			sdl_create_window $title, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_OPENGL
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
			vp_cpy [r4 + klocal_user], r0
			vp_lea [r0 + local_x_pos], r1
			vp_lea [r0 + local_y_pos], r2
			sdl_get_mouse_state r1, r2
			vp_cpy [r4 + klocal_user], r1
			vp_cpy r0, [r1 + local_buttons]

			;get keyboard state
			vp_add local_keymap_size, r1
			sdl_get_keyboard_state r1
			vp_cpy [r4 + klocal_user], r1
			vp_cpy r0, [r1 + local_keymap]

			;update the screen
			static_bind gui_gui, statics, r0
			s_call gui_gui, update, {[r0 + gui_statics_screen]}

			;refresh the window
			static_bind gui_gui, statics, r0
			sdl_render_present [r0 + gui_statics_renderer]
		endif

		vp_cpy [r4 + klocal_old_stack], r4
		vp_ret

	deinit_callback:
		;inputs
		;r0 = user data

		;free any screen
		static_bind gui_gui, statics, r5
		vp_cpy [r5 + gui_statics_screen], r0
		if r0, !=, 0
			vp_cpy_cl 0, [r5 + gui_statics_screen]
			s_call view, deref, {r0}
		endif

		;free old region
		static_bind gui_gui, statics, r5
		s_call gui_region, free, {&[r5 + gui_statics_rect_heap], &[r5 + gui_statics_old_region]}

		;deinit region heap
		s_call sys_heap, deinit, {r0}

		;deinit signal heap
		s_call sys_heap, deinit, {&[r5 + gui_statics_sigslot_heap]}

		;destroy any window
		vp_cpy [r5 + gui_statics_window], r14
		if r14, !=, 0
			;align stack on 16 byte boundary
			vp_cpy r4, r15
			vp_and -16, r4

			sdl_destroy_window r14
			ttf_quit
			sdl_quit

			vp_cpy r15, r4
		endif
		vp_ret

	title:
		db 'Asm Kernel GUI Window', 0

	scan_codes:
		db 4, 'aA', 5, 'bB', 6, 'cC', 7, 'dD', 8, 'eE', 9, 'fF', 10, 'gG', 11, 'hH'
		db 12, 'iI', 13, 'jJ', 14, 'kK', 15, 'lL', 16, 'mM', 17, 'nN', 18, 'oO', 19, 'pP'
		db 20, 'qQ', 21, 'rR', 22, 'sS', 23, 'tT', 24, 'uU', 25, 'vV', 26, 'wW', 27, 'xX'
		db 28, 'yY', 29, 'zZ', 30, '1!', 31, '2@', 32, '3#', 33, '4$', 34, '5%', 35, '6^'
		db 36, '7&', 37, '8*', 38, '9(', 39, '0)', 45, '-_', 46, '=+', 47, '[{', 48, ']}'
		db 49, '\|', 50, '  ', 51, ';:', 52, "'", '"', 53, '`~', 54, ',<', 55, '.>', 56, '/?'
		db 40, 13, 13	;return
		db 41, 27, 13	;escape
		db 42, 128, 128	;backspace
		db 43, 9, 9		;tab
		db 44, 32, 32	;space
		db 0xe1, 0, 0	;left shift
		db 0xe5, 0, 0	;right shift
		db 0x50, 0, 0	;left
		db 0x4f, 0, 0	;right
		db 0x52, 0, 0	;up
		db 0x51, 0, 0	;down
	scan_codes_end:

	fn_function_end
