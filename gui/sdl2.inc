(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'gui/gui.inc)

;;;;;;;;;;;;;
; sdl2 macros
;;;;;;;;;;;;;

(cond
	((eql *cpu* 'x86_64)
		(defmacro abi-args (&optional _) (setd _ 6) `',(slice 0 _ '(r6 r5 r2 r1 r7 r8 r0 r3 r4))))
	((eql *cpu* 'aarch64)
		(defmacro abi-args (&optional _) (setd _ 8) `',(slice 0 _ '(r0 r1 r2 r3 r4 r5 r6 r7 r8))))
	(t (throw "Unknown CPU !" *cpu*)))

(def-class 'sdl)
(def-method 'sdl_set_main_ready nil 'virtual (abi-args 0) '(r0))
(def-method 'sdl_init nil 'virtual (abi-args 1) '(r0))
(def-method 'sdl_get_error nil 'virtual (abi-args 0) '(r0))
(def-method 'sdl_quit nil 'virtual (abi-args 0) '(r0))
(def-method 'sdl_create_window nil 'virtual (abi-args 6) '(r0))
(def-method 'sdl_create_window_and_renderer nil 'virtual (abi-args 5) '(r0))
(def-method 'sdl_destroy_window nil 'virtual (abi-args 1) '(r0))
(def-method 'sdl_delay nil 'virtual (abi-args 1) '(r0))
(def-method 'sdl_create_renderer nil 'virtual (abi-args 3) '(r0))
(def-method 'sdl_set_render_draw_color nil 'virtual (abi-args 5) '(r0))
(def-method 'sdl_render_fill_rect nil 'virtual (abi-args 2) '(r0))
(def-method 'sdl_render_present nil 'virtual (abi-args 1) '(r0))
(def-method 'sdl_render_set_clip_rect nil 'virtual (abi-args 2) '(r0))
(def-method 'sdl_set_render_draw_blend_mode nil 'virtual (abi-args 2) '(r0))
(def-method 'sdl_pump_events nil 'virtual (abi-args 0) '(r0))
(def-method 'sdl_get_mouse_state nil 'virtual (abi-args 2) '(r0))
(def-method 'sdl_get_keyboard_state nil 'virtual (abi-args 1) '(r0))
(def-method 'sdl_render_draw_rect nil 'virtual (abi-args 2) '(r0))
(def-method 'sdl_free_surface nil 'virtual (abi-args 1) '(r0))
(def-method 'sdl_create_texture_from_surface nil 'virtual (abi-args 2) '(r0))
(def-method 'sdl_destroy_texture nil 'virtual (abi-args 1) '(r0))
(def-method 'sdl_render_copy nil 'virtual (abi-args 4) '(r0))
(def-method 'sdl_set_texture_blend_mode nil 'virtual (abi-args 2) '(r0))
(def-method 'sdl_set_texture_color_mod nil 'virtual (abi-args 4) '(r0))
(def-method 'sdl_create_rgb_surface_from nil 'virtual (abi-args 9) '(r0))
(def-method 'ttf_init nil 'virtual (abi-args 0) '(r0))
(def-method 'ttf_quit nil 'virtual (abi-args 0) '(r0))
(def-method 'ttf_open_font nil 'virtual (abi-args 2) '(r0))
(def-method 'ttf_close_font nil 'virtual (abi-args 1) '(r0))
(def-method 'ttf_size_utf8 nil 'virtual (abi-args 4) '(r0))
(def-method 'ttf_font_ascent nil 'virtual (abi-args 1) '(r0))
(def-method 'ttf_font_descent nil 'virtual (abi-args 1) '(r0))
(def-method 'ttf_font_height nil 'virtual (abi-args 1) '(r0))
(def-method 'ttf_render_utf8_blended nil 'virtual (abi-args 3) '(r0))

(defcfun abi-call (_1 _2 &optional _3 _4 _5)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	(setd _5 r14)
	(defq s 0 v 0 m nil)
	(method-lookup _1 _2)
	(if (eql (elem 2 m) 'static)
		(throw "Abi methods must be virtual !" (list _1 _2)))
	(setq s (elem 3 m))
	(if (find _5 s)
		(throw "Dispatch register conflicts with arg !" (list _5 s)))
	(assign _3 s)
	(when (gt (defq ls (length s)) (defq la (length (abi-args))))
		(defq x (slice la ls s))
		(if (ne 0 (bit-and (length x) 1)) (push x _5))
		(apply vp-push (map (lambda (__) (elem (sub (neg _) 2) x)) x)))
	(f-bind 'gui_gui 'statics _5)
	(vp-cpy-ir _5 gui_statics_sdl_funcs _5)
	(vp-call-i _5 v)
	(when (gt ls la)
		(vp-free (mul (length x) ptr_size)))
	(if _4 (assign (elem 4 m) _4)))

;;;;;;;;;;;;;;;;;
; sdl2 structures
;;;;;;;;;;;;;;;;;

(defcvar 'SDL_INIT_TIMER 0x00000001)
(defcvar 'SDL_INIT_AUDIO 0x00000010)
(defcvar 'SDL_INIT_VIDEO 0x00000020)
(defcvar 'SDL_INIT_JOYSTICK 0x00000200)
(defcvar 'SDL_INIT_HAPTIC 0x00001000)
(defcvar 'SDL_INIT_GAMECONTROLLER 0x00002000)
(defcvar 'SDL_INIT_EVENTS 0x00004000)
(defcvar 'SDL_INIT_NOPARACHUTE 0x00100000)
(defcvar 'SDL_INIT_EVERYTHING (bit-or SDL_INIT_TIMER SDL_INIT_AUDIO SDL_INIT_VIDEO SDL_INIT_EVENTS SDL_INIT_JOYSTICK SDL_INIT_HAPTIC SDL_INIT_GAMECONTROLLER))

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
(defcvar 'SDL_WINDOW_FULLSCREEN_DESKTOP (bit-or SDL_WINDOW_FULLSCREEN 0x00001000))
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
(defcvar 'SDL_BLENDMODE_MOD 3)

(def-struct 'sdl_rect)
	(int 'x)
	(int 'y)
	(int 'w)
	(int 'h)
(def-struct-end)

(def-struct 'sdl_surface)
	(int 'flags)
	(long 'format)
	(int 'w)
	(int 'h)
	(int 'pitch)
	(long 'pixels)
	(long 'userdata)
	(int 'locked)
	(long 'lockdata)
	(struct 'clip 'sdl_rect)
	(long 'map)
	(int 'ref)
(def-struct-end)

;sdl-set-main-ready
;sdl-init flags
;sdl-get-error
;sdl-delay time
;sdl-quit
;sdl-create-window title, x, y, w, h, flags
;sdl-create-window-and-renderer w, h, flags, window**, renderer**
;sdl-destroy-window window
;sdl-create-renderer window*, index, flags
;sdl-set-render-draw-color renderer**, r, g, b, a
;sdl-render-fill-rect renderer**, rect*
;sdl-render-draw-rect renderer**, rect*
;sdl-render-present renderer**
;sdl-render-set-clip-rect renderer**, rect*
;sdl-set-render-draw-blend-mode renderer**, mode
;sdl-pump-events
;sdl-get-mouse-state x*, y*
;sdl-get-keyboard-state int*
;sdl-free-surface surface*
;sdl-create-texture-from-surface renderer**, surface*
;sdl-create-rgb-surface-from pixels*, w, h, d, stride, rmask, gmask, bmask, amask
;sdl-destroy-texture texture*
;sdl-render-copy renderer**, texture*, srect*, drect*
;sdl-set-texture-blend-mode texture*, blendmode
;sdl-set-texture-color-mod texture*, byte r, byte g, byte b
;ttf-init
;ttf-quit
;ttf-open-font name*, pointsize
;ttf-close-font font*
;ttf-size-utf8 font*, text*, int *w, int *h
;ttf-font-ascent font*
;ttf-font-descent font*
;ttf-font-height font*
;ttf-render-utf8-blended font*, text*, color
