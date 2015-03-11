%include "func.inc"
%include "sdl2.inc"

	fn_function "sys/gui"

		;init sdl2
		sdl_setmainready
		sdl_init SDL_INIT_VIDEO

		;create window
		vp_lea [rel title], r14
		sdl_createwindow r14, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1024, 768, SDL_WINDOW_OPENGL
		vp_cpy r0, r14

		;wait 1 second
		sdl_delay 1000

		;destroy window
		sdl_destroywindow r14

		;deinit sdl2
		sdl_quit

		;stop this task
		fn_jmp sys/task_stop

	title:
		db "Test Window", 0

	fn_function_end
