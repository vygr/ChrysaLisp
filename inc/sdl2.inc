(import 'inc/code.inc)

;;;;;;;;;;;;;;;;;
; sdl2 structures
;;;;;;;;;;;;;;;;;

(def-struct 'sdl)
	(byte 'SDL_SetMainReady)
	(byte 'SDL_Init)
	(byte 'SDL_Quit)
	(byte 'SDL_CreateWindow)
	(byte 'SDL_CreateWindowAndRenderer)
	(byte 'SDL_DestroyWindow)
	(byte 'SDL_Delay)
	(byte 'SDL_CreateRenderer)
	(byte 'SDL_SetRenderDrawColor)
	(byte 'SDL_RenderFillRect)
	(byte 'SDL_RenderPresent)
	(byte 'SDL_RenderSetClipRect)
	(byte 'SDL_SetRenderDrawBlendMode)
	(byte 'SDL_PumpEvents)
	(byte 'SDL_GetMouseState)
	(byte 'SDL_GetKeyboardState)
	(byte 'SDL_RenderDrawRect)
	(byte 'SDL_FreeSurface)
	(byte 'SDL_CreateTextureFromSurface)
	(byte 'SDL_DestroyTexture)
	(byte 'SDL_RenderCopy)
	(byte 'SDL_SetTextureBlendMode)
	(byte 'SDL_SetTextureColorMod)

	(byte 'TTF_Init)
	(byte 'TTF_Quit)
	(byte 'TTF_OpenFont)
	(byte 'TTF_CloseFont)
	(byte 'TTF_SizeUTF8)
	(byte 'TTF_FontAscent)
	(byte 'TTF_FontDescent)
	(byte 'TTF_FontHeight)
	(byte 'TTF_RenderUTF8_Blended)
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

(defcfun sdl-init (_1)
	;flags
	(assign `(,_1) '(r7))
	(sdl-call sdl_SDL_Init))

(defcfun sdl-delay (_1)
	;time
	(assign `(,_1) '(r7))
	(sdl-call sdl_SDL_Delay))

(defcfun sdl-quit ()
	(sdl-call sdl_SDL_Quit))

(defcfun sdl-create-window (_1 _2 _3 _4 _5 _6)
	;title, x, y, w, h, flags
	(assign `(,_1 ,_2 ,_3 ,_4 ,_5 ,_6) '(r7 r6 r2 r1 r8 r9))
	(sdl-call sdl_SDL_CreateWindow))

(defcfun sdl-create-window-and-renderer (_1 _2 _3 _4 _5)
	;w, h, flags, window**, renderer**
	(assign `(,_1 ,_2 ,_3 ,_4 ,_5) '(r7 r6 r2 r1 r8))
	(sdl-call sdl_SDL_CreateWindowAndRenderer))

(defcfun sdl-destroy-window (_1)
	;window
	(assign `(,_1) '(r7))
	(sdl-call sdl_SDL_DestroyWindow))

(defcfun sdl-create-renderer (_1 _2 _3)
	;window*, index, flags
	(assign `(,_1 ,_2 ,_3) '(r7 r6 r2))
	(sdl-call sdl_SDL_CreateRenderer))

(defcfun sdl-set-render-draw-color (_1 _2 _3 _4 _5)
	;renderer**, r, g, b, a
	(assign `(,_1 ,_2 ,_3 ,_4 ,_5) '(r7 r6 r2 r1 r8))
	(sdl-call sdl_SDL_SetRenderDrawColor))

(defcfun sdl-render-fill-rect (_1 _2)
	;renderer**, rect*
	(assign `(,_1 ,_2) '(r7 r6))
	(sdl-call sdl_SDL_RenderFillRect))

(defcfun sdl-render-draw-rect (_1 _2)
	;renderer**, rect*
	(assign `(,_1 ,_2) '(r7 r6))
	(sdl-call sdl_SDL_RenderDrawRect))

(defcfun sdl-render-present (_1)
	;renderer**
	(assign `(,_1) '(r7))
	(sdl-call sdl_SDL_RenderPresent))

(defcfun sdl-render-set-clip-rect (_1 _2)
	;renderer**, rect*
	(assign `(,_1 ,_2) '(r7 r6))
	(sdl-call sdl_SDL_RenderSetClipRect))

(defcfun sdl-set-render-draw-blend-mode (_1 _2)
	;renderer**, mode
	(assign `(,_1 ,_2) '(r7 r6))
	(sdl-call sdl_SDL_SetRenderDrawBlendMode))

(defcfun sdl-pump-events ()
	(sdl-call sdl_SDL_PumpEvents))

(defcfun sdl-get-mouse-state (_1 _2)
	;x*, y*
	(assign `(,_1 ,_2) '(r7 r6))
	(sdl-call sdl_SDL_GetMouseState))

(defcfun sdl-get-keyboard-state (_1)
	;*int
	(assign `(,_1) '(r7))
	(sdl-call sdl_SDL_GetKeyboardState))

(defcfun sdl-free-surface (_1)
	;surface*
	(assign `(,_1) '(r7))
	(sdl-call sdl_SDL_FreeSurface))

(defcfun sdl-create-texture-from-surface (_1 _2)
	;renderer**, surface*
	(assign `(,_1 ,_2) '(r7 r6))
	(sdl-call sdl_SDL_CreateTextureFromSurface))

(defcfun sdl-destroy-texture (_1)
	;texture*
	(assign `(,_1) '(r7))
	(sdl-call sdl_SDL_DestroyTexture))

(defcfun sdl-render-copy (_1 _2 _3 _4)
	;renderer**, texture*, srect*, drect*
	(assign `(,_1 ,_2 ,_3 ,_4) '(r7 r6 r2 r1))
	(sdl-call sdl_SDL_RenderCopy))

(defcfun sdl-set-texture-blend-mode (_1 _2)
	;texture*, blendmode
	(assign `(,_1 ,_2) '(r7 r6))
	(sdl-call sdl_SDL_SetTextureBlendMode))

(defcfun sdl-set-texture-color-mod (_1 _2 _3 _4)
	;texture*, byte r, byte g, byte b
	(assign `(,_1 ,_2 ,_3 ,_4) '(r7 r6 r2 r1))
	(sdl-call sdl_SDL_SetTextureColorMod))

(defcfun ttf-init ()
	(sdl-call sdl_TTF_Init))

(defcfun ttf-quit ()
	(sdl-call sdl_TTF_Quit))

(defcfun ttf-open-font (_1 _2)
	;name*, pointsize
	(assign `(,_1 ,_2) '(r7 r6))
	(sdl-call sdl_TTF_OpenFont))

(defcfun ttf-close-font (_1)
	;font*
	(assign `(,_1) '(r7))
	(sdl-call sdl_TTF_CloseFont))

(defcfun ttf-size-utf8 (_1 _2 _3 _4)
	;font*, text*, int *w, int *h
	(assign `(,_1 ,_2 ,_3 ,_4) '(r7 r6 r2 r1))
	(sdl-call sdl_TTF_SizeUTF8))

(defcfun ttf-font-ascent (_1)
	;font*
	(assign `(,_1) '(r7))
	(sdl-call sdl_TTF_FontAscent))

(defcfun ttf-font-descent (_1)
	;font*
	(assign `(,_1) '(r7))
	(sdl-call sdl_TTF_FontDescent))

(defcfun ttf-font-height (_1)
	;font*
	(assign `(,_1) '(r7))
	(sdl-call sdl_TTF_FontHeight))

(defcfun ttf-render-utf8-blended (_1 _2 _3)
	;font*, text*, color
	(assign `(,_1 ,_2 ,_3) '(r7 r6 r2))
	(sdl-call sdl_TTF_RenderUTF8_Blended))
