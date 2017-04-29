(import 'inc/code.inc)
(import 'inc/gui.inc)

;;;;;;;;;;;;;;;;;
; sdl2 structures
;;;;;;;;;;;;;;;;;

(def-enum 'sdl)
	(enum
		'SDL_SetMainReady
		'SDL_Init
		'SDL_Quit
		'SDL_CreateWindow
		'SDL_CreateWindowAndRenderer
		'SDL_DestroyWindow
		'SDL_Delay
		'SDL_CreateRenderer
		'SDL_SetRenderDrawColor
		'SDL_RenderFillRect
		'SDL_RenderPresent
		'SDL_RenderSetClipRect
		'SDL_SetRenderDrawBlendMode
		'SDL_PumpEvents
		'SDL_GetMouseState
		'SDL_GetKeyboardState
		'SDL_RenderDrawRect
		'SDL_FreeSurface
		'SDL_CreateTextureFromSurface
		'SDL_DestroyTexture
		'SDL_RenderCopy
		'SDL_SetTextureBlendMode
		'SDL_SetTextureColorMod

		'TTF_Init
		'TTF_Quit
		'TTF_OpenFont
		'TTF_CloseFont
		'TTF_SizeUTF8
		'TTF_FontAscent
		'TTF_FontDescent
		'TTF_FontHeight
		'TTF_RenderUTF8_Blended)
(def-struct-end)

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

;;;;;;;;;;;;;
; sdl2 macros
;;;;;;;;;;;;;

(defcfun sdl-call (_1)
	(f-bind 'gui_gui 'statics r0)
	(vp-cpy-ir r0 gui_statics_sdl_funcs r0)
	(vp-call-i r0 (mul _1 ptr_size)))

(defcfun sdl-set-main-ready ()
	(sdl-call sdl_SDL_SetMainReady))

(defcfun sdl-init (_)
	;flags
	(assign _ '(r7))
	(sdl-call sdl_SDL_Init))

(defcfun sdl-delay (_)
	;time
	(assign _ '(r7))
	(sdl-call sdl_SDL_Delay))

(defcfun sdl-quit ()
	(sdl-call sdl_SDL_Quit))

(defcfun sdl-create-window (_)
	;title, x, y, w, h, flags
	(assign _ '(r7 r6 r2 r1 r8 r9))
	(sdl-call sdl_SDL_CreateWindow))

(defcfun sdl-create-window-and-renderer (_)
	;w, h, flags, window**, renderer**
	(assign _ '(r7 r6 r2 r1 r8))
	(sdl-call sdl_SDL_CreateWindowAndRenderer))

(defcfun sdl-destroy-window (_)
	;window
	(assign _ '(r7))
	(sdl-call sdl_SDL_DestroyWindow))

(defcfun sdl-create-renderer (_)
	;window*, index, flags
	(assign _ '(r7 r6 r2))
	(sdl-call sdl_SDL_CreateRenderer))

(defcfun sdl-set-render-draw-color (_)
	;renderer**, r, g, b, a
	(assign _ '(r7 r6 r2 r1 r8))
	(sdl-call sdl_SDL_SetRenderDrawColor))

(defcfun sdl-render-fill-rect (_)
	;renderer**, rect*
	(assign _ '(r7 r6))
	(sdl-call sdl_SDL_RenderFillRect))

(defcfun sdl-render-draw-rect (_)
	;renderer**, rect*
	(assign _ '(r7 r6))
	(sdl-call sdl_SDL_RenderDrawRect))

(defcfun sdl-render-present (_)
	;renderer**
	(assign _ '(r7))
	(sdl-call sdl_SDL_RenderPresent))

(defcfun sdl-render-set-clip-rect (_)
	;renderer**, rect*
	(assign _ '(r7 r6))
	(sdl-call sdl_SDL_RenderSetClipRect))

(defcfun sdl-set-render-draw-blend-mode (_)
	;renderer**, mode
	(assign _ '(r7 r6))
	(sdl-call sdl_SDL_SetRenderDrawBlendMode))

(defcfun sdl-pump-events ()
	(sdl-call sdl_SDL_PumpEvents))

(defcfun sdl-get-mouse-state (_)
	;x*, y*
	(assign _ '(r7 r6))
	(sdl-call sdl_SDL_GetMouseState))

(defcfun sdl-get-keyboard-state (_)
	;*int
	(assign _ '(r7))
	(sdl-call sdl_SDL_GetKeyboardState))

(defcfun sdl-free-surface (_)
	;surface*
	(assign _ '(r7))
	(sdl-call sdl_SDL_FreeSurface))

(defcfun sdl-create-texture-from-surface (_)
	;renderer**, surface*
	(assign _ '(r7 r6))
	(sdl-call sdl_SDL_CreateTextureFromSurface))

(defcfun sdl-destroy-texture (_)
	;texture*
	(assign _ '(r7))
	(sdl-call sdl_SDL_DestroyTexture))

(defcfun sdl-render-copy (_)
	;renderer**, texture*, srect*, drect*
	(assign _ '(r7 r6 r2 r1))
	(sdl-call sdl_SDL_RenderCopy))

(defcfun sdl-set-texture-blend-mode (_)
	;texture*, blendmode
	(assign _ '(r7 r6))
	(sdl-call sdl_SDL_SetTextureBlendMode))

(defcfun sdl-set-texture-color-mod (_)
	;texture*, byte r, byte g, byte b
	(assign _ '(r7 r6 r2 r1))
	(sdl-call sdl_SDL_SetTextureColorMod))

(defcfun ttf-init ()
	(sdl-call sdl_TTF_Init))

(defcfun ttf-quit ()
	(sdl-call sdl_TTF_Quit))

(defcfun ttf-open-font (_)
	;name*, pointsize
	(assign _ '(r7 r6))
	(sdl-call sdl_TTF_OpenFont))

(defcfun ttf-close-font (_)
	;font*
	(assign _ '(r7))
	(sdl-call sdl_TTF_CloseFont))

(defcfun ttf-size-utf8 (_)
	;font*, text*, int *w, int *h
	(assign _ '(r7 r6 r2 r1))
	(sdl-call sdl_TTF_SizeUTF8))

(defcfun ttf-font-ascent (_)
	;font*
	(assign _ '(r7))
	(sdl-call sdl_TTF_FontAscent))

(defcfun ttf-font-descent (_)
	;font*
	(assign _ '(r7))
	(sdl-call sdl_TTF_FontDescent))

(defcfun ttf-font-height (_)
	;font*
	(assign _ '(r7))
	(sdl-call sdl_TTF_FontHeight))

(defcfun ttf-render-utf8-blended (_)
	;font*, text*, color
	(assign _ '(r7 r6 r2))
	(sdl-call sdl_TTF_RenderUTF8_Blended))
