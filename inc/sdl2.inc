(import 'inc/vp.inc)
(import 'inc/code.inc)
(import 'inc/gui.inc)
(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;
; sdl2 structures
;;;;;;;;;;;;;;;;;

(def-struct 'sdl)
	(byte 'sdl_SDL_SetMainReady)
	(byte 'sdl_SDL_Init)
	(byte 'sdl_SDL_Quit)
	(byte 'sdl_SDL_CreateWindow)
	(byte 'sdl_SDL_CreateWindowAndRenderer)
	(byte 'sdl_SDL_DestroyWindow)
	(byte 'sdl_SDL_Delay)
	(byte 'sdl_SDL_CreateRenderer)
	(byte 'sdl_SDL_SetRenderDrawColor)
	(byte 'sdl_SDL_RenderFillRect)
	(byte 'sdl_SDL_RenderPresent)
	(byte 'sdl_SDL_RenderSetClipRect)
	(byte 'sdl_SDL_SetRenderDrawBlendMode)
	(byte 'sdl_SDL_PumpEvents)
	(byte 'sdl_SDL_GetMouseState)
	(byte 'sdl_SDL_GetKeyboardState)
	(byte 'sdl_SDL_RenderDrawRect)
	(byte 'sdl_SDL_FreeSurface)
	(byte 'sdl_SDL_CreateTextureFromSurface)
	(byte 'sdl_SDL_DestroyTexture)
	(byte 'sdl_SDL_RenderCopy)
	(byte 'sdl_SDL_SetTextureBlendMode)
	(byte 'sdl_SDL_SetTextureColorMod)

	(byte 'sdl_TTF_Init)
	(byte 'sdl_TTF_Quit)
	(byte 'sdl_TTF_OpenFont)
	(byte 'sdl_TTF_CloseFont)
	(byte 'sdl_TTF_SizeUTF8)
	(byte 'sdl_TTF_FontAscent)
	(byte 'sdl_TTF_FontDescent)
	(byte 'sdl_TTF_FontHeight)
	(byte 'sdl_TTF_RenderUTF8_Blended)
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
	(int 'sdl_rect_x)
	(int 'sdl_rect_y)
	(int 'sdl_rect_w)
	(int 'sdl_rect_h)
(def-struct-end)

(def-struct 'sdl_surface)
	(int 'sdl_surface_flags)
	(long 'sdl_surface_format)
	(int 'sdl_surface_w)
	(int 'sdl_surface_h)
	(int 'sdl_surface_pitch)
	(long 'sdl_surface_pixels)
	(long 'sdl_surface_userdata)
	(int 'sdl_surface_locked)
	(long 'sdl_surface_lockdata)
	(struct 'sdl_surface_clip 'sdl_rect)
	(long 'sdl_surface_map)
	(int 'sdl_surface_ref)
(def-struct-end)

;;;;;;;;;;;;;
; sdl2 macros
;;;;;;;;;;;;;

(defcfun sdl-call (%1)
	(f-bind 'gui_gui 'statics r0)
	(vp-cpy-ir r0 gui_statics_sdl_funcs r0)
	(vp-call-i r0 (mul %1 ptr_size)))

(defcfun sdl-set-main-ready ()
	(sdl-call sdl_SDL_SetMainReady))

(defcfun sdl-init (%1)
	;flags
	(assign `(,%1) '(r7))
	(sdl-call sdl_SDL_Init))

(defcfun sdl-delay (%1)
	;time
	(assign `(,%1) '(r7))
	(sdl-call sdl_SDL_Delay))

(defcfun sdl-quit ()
	(sdl-call sdl_SDL_Quit))

(defcfun sdl-create-window (%1 %2 %3 %4 %5 %6)
	;title, x, y, w, h, flags
	(assign `(,%1 ,%2 ,%3 ,%4 ,%5 ,%6) '(r7 r6 r2 r1 r8 r9))
	(sdl-call sdl_SDL_CreateWindow))

(defcfun sdl-create-window-and-renderer (%1 %2 %3 %4 %5)
	;w, h, flags, window**, renderer**
	(assign `(,%1 ,%2 ,%3 ,%4 ,%5) '(r7 r6 r2 r1 r8))
	(sdl-call sdl_SDL_CreateWindowAndRenderer))

(defcfun sdl-destroy-window (%1)
	;window
	(assign `(,%1) '(r7))
	(sdl-call sdl_SDL_DestroyWindow))

(defcfun sdl-create-renderer (%1 %2 %3)
	;window*, index, flags
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sdl-call sdl_SDL_CreateRenderer))

(defcfun sdl-set-render-draw-color (%1 %2 %3 %4 %5)
	;renderer**, r, g, b, a
	(assign `(,%1 ,%2 ,%3 ,%4 ,%5) '(r7 r6 r2 r1 r8))
	(sdl-call sdl_SDL_SetRenderDrawColor))

(defcfun sdl-render-fill-rect (%1 %2)
	;renderer**, rect*
	(assign `(,%1 ,%2) '(r7 r6))
	(sdl-call sdl_SDL_RenderFillRect))

(defcfun sdl-render-draw-rect (%1 %2)
	;renderer**, rect*
	(assign `(,%1 ,%2) '(r7 r6))
	(sdl-call sdl_SDL_RenderDrawRect))

(defcfun sdl-render-present (%1)
	;renderer**
	(assign `(,%1) '(r7))
	(sdl-call sdl_SDL_RenderPresent))

(defcfun sdl-render-set-clip-rect (%1 %2)
	;renderer**, rect*
	(assign `(,%1 ,%2) '(r7 r6))
	(sdl-call sdl_SDL_RenderSetClipRect))

(defcfun sdl-set-render-draw-blend-mode (%1 %2)
	;renderer**, mode
	(assign `(,%1 ,%2) '(r7 r6))
	(sdl-call sdl_SDL_SetRenderDrawBlendMode))

(defcfun sdl-pump-events ()
	(sdl-call sdl_SDL_PumpEvents))

(defcfun sdl-get-mouse-state (%1 %2)
	;x*, y*
	(assign `(,%1 ,%2) '(r7 r6))
	(sdl-call sdl_SDL_GetMouseState))

(defcfun sdl-get-keyboard-state (%1)
	;*int
	(assign `(,%1) '(r7))
	(sdl-call sdl_SDL_GetKeyboardState))

(defcfun sdl-free-surface (%1)
	;surface*
	(assign `(,%1) '(r7))
	(sdl-call sdl_SDL_FreeSurface))

(defcfun sdl-create-texture-from-surface (%1 %2)
	;renderer**, surface*
	(assign `(,%1 ,%2) '(r7 r6))
	(sdl-call sdl_SDL_CreateTextureFromSurface))

(defcfun sdl-destroy-texture (%1)
	;texture*
	(assign `(,%1) '(r7))
	(sdl-call sdl_SDL_DestroyTexture))

(defcfun sdl-render-copy (%1 %2 %3 %4)
	;renderer**, texture*, srect*, drect*
	(assign `(,%1 ,%2 ,%3 ,%4) '(r7 r6 r2 r1))
	(sdl-call sdl_SDL_RenderCopy))

(defcfun sdl-set-texture-blend-mode (%1 %2)
	;texture*, blendmode
	(assign `(,%1 ,%2) '(r7 r6))
	(sdl-call sdl_SDL_SetTextureBlendMode))

(defcfun sdl-set-texture-color-mod (%1 %2 %3 %4)
	;texture*, byte r, byte g, byte b
	(assign `(,%1 ,%2 ,%3 ,%4) '(r7 r6 r2 r1))
	(sdl-call sdl_SDL_SetTextureColorMod))

(defcfun ttf-init ()
	(sdl-call sdl_TTF_Init))

(defcfun ttf-quit ()
	(sdl-call sdl_TTF_Quit))

(defcfun ttf-open-font (%1 %2)
	;name*, pointsize
	(assign `(,%1 ,%2) '(r7 r6))
	(sdl-call sdl_TTF_OpenFont))

(defcfun ttf-close-font (%1)
	;font*
	(assign `(,%1) '(r7))
	(sdl-call sdl_TTF_CloseFont))

(defcfun ttf-size-utf8 (%1 %2 %3 %4)
	;font*, text*, int *w, int *h
	(assign `(,%1 ,%2 ,%3 ,%4) '(r7 r6 r2 r1))
	(sdl-call sdl_TTF_SizeUTF8))

(defcfun ttf-font-ascent (%1)
	;font*
	(assign `(,%1) '(r7))
	(sdl-call sdl_TTF_FontAscent))

(defcfun ttf-font-descent (%1)
	;font*
	(assign `(,%1) '(r7))
	(sdl-call sdl_TTF_FontDescent))

(defcfun ttf-font-height (%1)
	;font*
	(assign `(,%1) '(r7))
	(sdl-call sdl_TTF_FontHeight))

(defcfun ttf-render-utf8-blended (%1 %2 %3)
	;font*, text*, color
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sdl-call sdl_TTF_RenderUTF8_Blended))
