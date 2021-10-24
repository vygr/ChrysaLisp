(include "sys/class.inc")
(include "sys/pii/abi.inc")

(def-class host nil
	(dec-method :exit nil :virtual (abi-args 1))
	(dec-method :stat nil :virtual (abi-args 2) (:r0))
	(dec-method :open nil :virtual (abi-args 2) (:r0))
	(dec-method :close nil :virtual (abi-args 1) (:r0))
	(dec-method :unlink nil :virtual (abi-args 1) (:r0))
	(dec-method :read nil :virtual (abi-args 3) (:r0))
	(dec-method :write nil :virtual (abi-args 3) (:r0))
	(dec-method :mmap nil :virtual (abi-args 3) (:r0))
	(dec-method :munmap nil :virtual (abi-args 3) (:r0))
	(dec-method :mprotect nil :virtual (abi-args 3) (:r0))
	(dec-method :gettime nil :virtual () (:r0))
	(dec-method :open_shared nil :virtual (abi-args 2) (:r0))
	(dec-method :close_shared nil :virtual (abi-args 2) (:r0))
	(dec-method :clear_icache nil :virtual (abi-args 2) (:r0))
	(dec-method :dirlist nil :virtual (abi-args 3) (:r0))
	(dec-method :remove nil :virtual (abi-args 1) (:r0))
	(dec-method :seek nil :virtual (abi-args 3) (:r0))
	(dec-method :rand nil :virtual (abi-args 2) (:r0))
	(dec-method :sleep nil :virtual (abi-args 1) (:r0))
	(dec-method :usb_start nil :virtual (abi-args 1) (:r0))
	(dec-method :usb_stop nil :virtual (abi-args 1) (:r0))
	(dec-method :usb_running nil :virtual (abi-args 1) (:r0)))

(def-class host_gui nil
	(dec-method :sdl_set_main_ready nil :virtual (abi-args 0) (:r0))
	(dec-method :sdl_init nil :virtual (abi-args 1) (:r0))
	(dec-method :sdl_get_error nil :virtual (abi-args 0) (:r0))
	(dec-method :sdl_quit nil :virtual (abi-args 0) (:r0))
	(dec-method :sdl_create_window nil :virtual (abi-args 6) (:r0))
	(dec-method :sdl_create_window_and_renderer nil :virtual (abi-args 5) (:r0))
	(dec-method :sdl_destroy_window nil :virtual (abi-args 1) (:r0))
	(dec-method :sdl_create_renderer nil :virtual (abi-args 3) (:r0))
	(dec-method :sdl_set_render_draw_color nil :virtual (abi-args 5) (:r0))
	(dec-method :sdl_render_fill_rect nil :virtual (abi-args 2) (:r0))
	(dec-method :sdl_render_present nil :virtual (abi-args 1) (:r0))
	(dec-method :sdl_render_set_clip_rect nil :virtual (abi-args 2) (:r0))
	(dec-method :sdl_set_render_draw_blend_mode nil :virtual (abi-args 2) (:r0))
	(dec-method :sdl_poll_event nil :virtual (abi-args 1) (:r0))
	(dec-method :sdl_render_draw_rect nil :virtual (abi-args 2) (:r0))
	(dec-method :sdl_free_surface nil :virtual (abi-args 1) (:r0))
	(dec-method :sdl_create_texture_from_surface nil :virtual (abi-args 2) (:r0))
	(dec-method :sdl_destroy_texture nil :virtual (abi-args 1) (:r0))
	(dec-method :sdl_render_copy nil :virtual (abi-args 4) (:r0))
	(dec-method :sdl_set_texture_blend_mode nil :virtual (abi-args 2) (:r0))
	(dec-method :sdl_set_texture_color_mod nil :virtual (abi-args 4) (:r0))
	(dec-method :sdl_create_rgb_surface_from nil :virtual (abi-args 9) (:r0))
	(dec-method :sdl_compose_custom_blend_mode nil :virtual (abi-args 6) (:r0))
	(dec-method :sdl_create_texture nil :virtual (abi-args 5) (:r0))
	(dec-method :sdl_set_render_target nil :virtual (abi-args 2) (:r0))
	(dec-method :sdl_render_clear nil :virtual (abi-args 1) (:r0))
	(dec-method :sdl_show_cursor nil :virtual (abi-args 1) (:r0)))

(defun abi-call-table (_1 _2 _5 &optional _3 _4 _6)
	;_1 = class name
	;_2 = member name
	;_5 = host function table
	;_3 = in parameters
	;_4 = out parameters
	;_6 = temp reg
	(setd _6 :r13)
	(defq m (method-lookup _1 _2) x (list) s (elem 2 m))
	(if (eql (elem 1 m) :static)
		(throw "Abi methods must be virtual !" (list _1 _2)))
	(if (or (find-rev _5 s) (find-rev _6 s))
		(throw "Dispatch/Temp register conflicts with arg !" (list _5 _6 s)))
	(assign _3 s)
	(when (> (defq ls (length s)) (defq la (length (abi-args))))
		(setq x (reverse (slice la ls s))))
	(vp-call-abi _6 _5 (- (elem 4 m) +ptr_size) (length s) x)
	(if _4 (assign (elem 3 m) _4)))

(defun host-os-call (_1 _2 &optional _3 _4 _5 _6)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	;_6 = temp reg
	(setd _5 :r14 _6 :r13)
	(fn-bind 'sys/statics/statics _5)
	(vp-cpy-ir _5 statics_sys_load_host_os_funcs _5)
	(abi-call-table _1 _2 _5 _3 _4 _6))

(defun host-gui-call (_1 _2 &optional _3 _4 _5 _6)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	;_6 = temp reg
	(setd _5 :r14 _6 :r13)
	(fn-bind 'sys/statics/statics _5)
	(vp-cpy-ir _5 statics_sys_load_host_gui_funcs _5)
	(abi-call-table _1 _2 _5 _3 _4 _6))

;;;;;;;;;;;;;;;
; SDL interface
;;;;;;;;;;;;;;;

(defcvar 'SDL_INIT_TIMER 0x00000001)
(defcvar 'SDL_INIT_AUDIO 0x00000010)
(defcvar 'SDL_INIT_VIDEO 0x00000020)
(defcvar 'SDL_INIT_JOYSTICK 0x00000200)
(defcvar 'SDL_INIT_HAPTIC 0x00001000)
(defcvar 'SDL_INIT_GAMECONTROLLER 0x00002000)
(defcvar 'SDL_INIT_EVENTS 0x00004000)
(defcvar 'SDL_INIT_NOPARACHUTE 0x00100000)
(defcvar 'SDL_INIT_EVERYTHING (logior SDL_INIT_TIMER SDL_INIT_AUDIO SDL_INIT_VIDEO SDL_INIT_EVENTS SDL_INIT_JOYSTICK SDL_INIT_HAPTIC SDL_INIT_GAMECONTROLLER))

(defcvar 'SDL_WINDOW_FULLSCREEN 0x00000001)
(defcvar 'SDL_WINDOW_OPENGL 0x00000002)
(defcvar 'SDL_WINDOW_SHOWN 0x00000004)
(defcvar 'SDL_WINDOW_HIDDEN 0x00000008)
(defcvar 'SDL_WINDOW_BORDERLESS 0x00000010)
(defcvar 'SDL_WINDOW_RESIZABLE 0x00000020)
(defcvar 'SDL_WINDOW_MINIMIZED 0x00000040)
(defcvar 'SDL_WINDOW_MAXIMIZED 0x00000080)
(defcvar 'SDL_WINDOW_INPUT_GRABBED 0x00000100)
(defcvar 'SDL_WINDOW_INPUT_FOCUS 0x00000200)
(defcvar 'SDL_WINDOW_MOUSE_FOCUS 0x00000400)
(defcvar 'SDL_WINDOW_FULLSCREEN_DESKTOP (logior SDL_WINDOW_FULLSCREEN 0x00001000))
(defcvar 'SDL_WINDOW_FOREIGN 0x00000800)
(defcvar 'SDL_WINDOW_ALLOW_HIGHDPI 0x00002000)

(defcvar 'SDL_WINDOWPOS_UNDEFINED_MASK 0x1FFF0000)
(defcvar 'SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED_MASK)
(defcvar 'SDL_WINDOWPOS_CENTERED_MASK 0x2FFF0000)

(defcvar 'SDL_RENDERER_SOFTWARE 0x00000001)
(defcvar 'SDL_RENDERER_ACCELERATED 0x00000002)
(defcvar 'SDL_RENDERER_PRESENTVSYNC 0x00000004)
(defcvar 'SDL_RENDERER_TARGETTEXTURE 0x00000008)

(defcvar 'SDL_BLENDMODE_NONE 0)
(defcvar 'SDL_BLENDMODE_BLEND 1)
(defcvar 'SDL_BLENDMODE_ADD 2)
(defcvar 'SDL_BLENDMODE_MOD 4)

(defcvar 'SDL_BLENDOPERATION_ADD 0x1)
(defcvar 'SDL_BLENDOPERATION_SUBTRACT 0x2)
(defcvar 'SDL_BLENDOPERATION_REV_SUBTRACT 0x3)
(defcvar 'SDL_BLENDOPERATION_MINIMUM 0x4)
(defcvar 'SDL_BLENDOPERATION_MAXIMUM 0x5)

(defcvar 'SDL_BLENDFACTOR_ZERO 0x1)
(defcvar 'SDL_BLENDFACTOR_ONE 0x2)
(defcvar 'SDL_BLENDFACTOR_SRC_COLOR 0x3)
(defcvar 'SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR 0x4)
(defcvar 'SDL_BLENDFACTOR_SRC_ALPHA 0x5)
(defcvar 'SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA 0x6)
(defcvar 'SDL_BLENDFACTOR_DST_COLOR 0x7)
(defcvar 'SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR 0x8)
(defcvar 'SDL_BLENDFACTOR_DST_ALPHA 0x9)
(defcvar 'SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA 0xA)

(defcvar 'SDL_TEXTUREACCESS_STATIC 0x0)
(defcvar 'SDL_TEXTUREACCESS_STREAMING 0x1)
(defcvar 'SDL_TEXTUREACCESS_TARGET 0x2)

(defcvar 'SDL_PIXELFORMAT_RGB888 0x16161804)
(defcvar 'SDL_PIXELFORMAT_RGBA8888 0x16462004)
(defcvar 'SDL_PIXELFORMAT_ARGB8888 0x16362004)

(def-struct sdl_rect 0
	(int x y w h))

(def-struct sdl_surface 0
	(int flags)
	(long format)
	(int w h pitch)
	(long pixels userdata)
	(int locked)
	(long lockdata)
	(struct clip sdl_rect_size)
	(long map)
	(int obj))

(def-class sys_pii nil
	(dec-method :exit sys/pii/exit :static (:r0))
	(dec-method :mmap sys/pii/mmap :static (:r0 :r1 :r2) (:r0))
	(dec-method :munmap sys/pii/munmap :static (:r0 :r1 :r2) (:r0))
	(dec-method :mprotect sys/pii/mprotect :static (:r0 :r1 :r2) (:r0))
	(dec-method :open sys/pii/open :static (:r0 :r1) (:r0))
	(dec-method :close sys/pii/close :static (:r0) (:r0))
	(dec-method :open_shared sys/pii/open_shared :static (:r0 :r1) (:r0))
	(dec-method :close_shared sys/pii/close_shared :static (:r0 :r1) (:r0))
	(dec-method :unlink sys/pii/unlink :static (:r0) (:r0))
	(dec-method :stat sys/pii/stat :static (:r0 :r1) (:r0))
	(dec-method :write sys/pii/write :static (:r0 :r1 :r2) (:r0))
	(dec-method :write_char sys/pii/write_char :static (:r0 :r1) (:r0))
	(dec-method :write_str sys/pii/write_str :static (:r0 :r1) (:r0))
	(dec-method :write_num sys/pii/write_num :static (:r0 :r1 :r2))
	(dec-method :read sys/pii/read :static (:r0 :r1 :r2) (:r0))
	(dec-method :read_char sys/pii/read_char :static (:r0) (:r0))
	(dec-method :time sys/pii/time :static nil (:r0))
	(dec-method :clear_icache sys/pii/clear_icache :static (:r0 :r1) (:r0))
	(dec-method :dirlist sys/pii/dirlist :static (:r0 :r1 :r2) (:r0))
	(dec-method :remove sys/pii/remove :static (:r0) (:r0))
	(dec-method :seek sys/pii/seek :static (:r0 :r1 :r2) (:r0))
	(dec-method :rand sys/pii/rand :static (:r0 :r1) (:r0))
	(dec-method :usb_start sys/pii/usb_start :static (:r0) (:r0))
	(dec-method :usb_stop sys/pii/usb_stop :static (:r0) (:r0))
	(dec-method :usb_running sys/pii/usb_running :static (:r0) (:r0))
	(dec-method :lisp_readchar sys/pii/lisp_readchar :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_writechar sys/pii/lisp_writechar :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_time sys/pii/lisp_time :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_age sys/pii/lisp_age :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_dirlist sys/pii/lisp_dirlist :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_fstat sys/pii/lisp_fstat :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_remove sys/pii/lisp_remove :static (:r0 :r1) (:r0 :r1)))

;debug helpers
(defun debug-num (s n)
	(when (> *debug_mode* 0)
		(def-vars
			(ptr _a _b _c))
		(push-scope)
		(assign '(:r0 :r1 :r2) {_a, _b, _c})
		(fn-string (cat s ": ") :r1)
		(call 'sys_pii :write_str '(2 :r1))
		(assign {_a, _b, _c} '(:r0 :r1 :r2))
		(assign n '(:r1))
		(call 'sys_pii :write_num '(2 :r1 16))
		(call 'sys_pii :write_char '(2 10))
		(assign {_a, _b, _c} '(:r0 :r1 :r2))
		(pop-scope)))

(defun debug-str (s)
	(when (> *debug_mode* 0)
		(def-vars
			(ptr _a _b))
		(push-scope)
		(assign '(:r0 :r1) {_a, _b})
		(assign s '(:r1))
		(call 'sys_pii :write_str '(2 :r1))
		(call 'sys_pii :write_char '(2 10))
		(assign {_a, _b} '(:r0 :r1))
		(pop-scope)))

(def-enum +file_open 0
	(enum read write append))

(def-enum +mmap 0
	(enum data exec shared none))

(def-struct stat 0
	(long mtime fsize)
	(ushort mode))

(defcvar 's_ifmt 0xf000)
(defcvar 's_ifdir 0x4000)
(defcvar 's_ifreg 0x8000)
