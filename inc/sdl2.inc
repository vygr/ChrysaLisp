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

(equate 'SDL_INIT_TIMER 0x00000001)
(equate 'SDL_INIT_AUDIO 0x00000010)
(equate 'SDL_INIT_VIDEO 0x00000020)
(equate 'SDL_INIT_JOYSTICK 0x00000200)
(equate 'SDL_INIT_HAPTIC 0x00001000)
(equate 'SDL_INIT_GAMECONTROLLER 0x00002000)
(equate 'SDL_INIT_EVENTS 0x00004000)
(equate 'SDL_INIT_NOPARACHUTE 0x00100000)
(equate 'SDL_INIT_EVERYTHING (bit-or SDL_INIT_TIMER SDL_INIT_AUDIO SDL_INIT_VIDEO SDL_INIT_EVENTS SDL_INIT_JOYSTICK SDL_INIT_HAPTIC SDL_INIT_GAMECONTROLLER))

(equate 'SDL_WINDOW_FULLSCREEN 0x00000001)
(equate 'SDL_WINDOW_OPENGL 0x00000002)
(equate 'SDL_WINDOW_SHOWN 0x00000004)
(equate 'SDL_WINDOW_HIDDEN 0x00000008)
(equate 'SDL_WINDOW_BORDERLESS 0x00000010)
(equate 'SDL_WINDOW_RESIZABLE 0x00000020)
(equate 'SDL_WINDOW_MINIMIZED 0x00000040)
(equate 'SDL_WINDOW_MAXIMIZED 0x00000080)
(equate 'SDL_WINDOW_INPUT_GRABBED 0x00000100)
(equate 'SDL_WINDOW_INPUT_FOCUS 0x00000200)
(equate 'SDL_WINDOW_MOUSE_FOCUS 0x00000400)
(equate 'SDL_WINDOW_FULLSCREEN_DESKTOP (bit-or SDL_WINDOW_FULLSCREEN 0x00001000))
(equate 'SDL_WINDOW_FOREIGN 0x00000800)
(equate 'SDL_WINDOW_ALLOW_HIGHDPI 0x00002000)

(equate 'SDL_WINDOWPOS_UNDEFINED_MASK 0x1FFF0000)
(equate 'SDL_WINDOWPOS_CENTERED_MASK 0x2FFF0000)

(equate 'SDL_RENDERER_SOFTWARE 0x00000001)
(equate 'SDL_RENDERER_ACCELERATED 0x00000002)
(equate 'SDL_RENDERER_PRESENTVSYNC 0x00000004)
(equate 'SDL_RENDERER_TARGETTEXTURE 0x00000008)

(equate 'SDL_BLENDMODE_NONE 0)
(equate 'SDL_BLENDMODE_BLEND 1)
(equate 'SDL_BLENDMODE_ADD 2)
(equate 'SDL_BLENDMODE_MOD 3)

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

(defcompilefun sdl-call (%1)
	(f-bind 'gui_gui 'statics r0)
	(vp-cpy-ir r0 gui_statics_sdl_funcs r0)
	(vp-call-i r0 (mul %1 ptr_size)))

(defcompilefun sdl_set_main_ready ()
	(sdl-call sdl_SDL_SetMainReady))

(defcompilefun sdl-init (%1)
	;flags
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_SDL_Init))

(defcompilefun sdl-delay (%1)
	;time
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_SDL_Delay))

(defcompilefun sdl_quit ()
	(sdl-call sdl_SDL_Quit))

(defcompilefun sdl-create-window (%1 %2 %3 %4 %5 %6)
	;title, x, y, w, h, flags
	(set-src `(,%1 ,%2 ,%3 ,%4 ,%5 ,%6))
	(set-dst '(r7 r6 r2 r1 r8 r9))
	(map-src-to-dst)
	(sdl-call sdl_SDL_CreateWindow))

(defcompilefun sdl-create-window-and-renderer (%1 %2 %3 %4 %5)
	;w, h, flags, window**, renderer**
	(set-src `(,%1 ,%2 ,%3 ,%4 ,%5))
	(set-dst '(r7 r6 r2 r1 r8))
	(map-src-to-dst)
	(sdl-call sdl_SDL_CreateWindowAndRenderer))

(defcompilefun sdl-destroy-window (%1)
	;window
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_SDL_DestroyWindow))

(defcompilefun sdl-create-renderer (%1 %2 %3)
	;window*, index, flags
	(set-src `(,%1 ,%2 ,%3))
	(set-dst '(r7 r6 r2))
	(map-src-to-dst)
	(sdl-call sdl_SDL_CreateRenderer))

(defcompilefun sdl-set-render-draw-color (%1 %2 %3 %4 %5)
	;renderer**, r, g, b, a
	(set-src `(,%1 ,%2 ,%3 ,%4 ,%5))
	(set-dst '(r7 r6 r2 r1 r8))
	(map-src-to-dst)
	(sdl-call sdl_SDL_SetRenderDrawColor))

(defcompilefun sdl-render-fill-rect (%1 %2)
	;renderer**, rect*
	(set-src `(,%1 ,%2))
	(set-dst '(r7 r6))
	(map-src-to-dst)
	(sdl-call sdl_SDL_RenderFillRect))

(defcompilefun sdl-render-draw-rect (%1 %2)
	;renderer**, rect*
	(set-src `(,%1 ,%2))
	(set-dst '(r7 r6))
	(map-src-to-dst)
	(sdl-call sdl_SDL_RenderDrawRect))

(defcompilefun sdl-render-present (%1)
	;renderer**
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_SDL_RenderPresent))

(defcompilefun sdl-render-set-clip-rect (%1 %2)
	;renderer**, rect*
	(set-src `(,%1 ,%2))
	(set-dst '(r7 r6))
	(map-src-to-dst)
	(sdl-call sdl_SDL_RenderSetClipRect))

(defcompilefun sdl-set-render-draw-blend-mode (%1 %2)
	;renderer**, mode
	(set-src `(,%1 ,%2))
	(set-dst '(r7 r6))
	(map-src-to-dst)
	(sdl-call sdl_SDL_SetRenderDrawBlendMode))

(defcompilefun sdl_pump_events ()
	(sdl-call sdl_SDL_PumpEvents))

(defcompilefun sdl-get-mouse-state (%1 %2)
	;x*, y*
	(set-src `(,%1 ,%2))
	(set-dst '(r7 r6))
	(map-src-to-dst)
	(sdl-call sdl_SDL_GetMouseState))

(defcompilefun sdl-get-keyboard-state (%1)
	;*int
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_SDL_GetKeyboardState))

(defcompilefun sdl-free-surface (%1)
	;surface*
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_SDL_FreeSurface))

(defcompilefun sdl-create-texture-from-surface (%1 %2)
	;renderer**, surface*
	(set-src `(,%1 ,%2))
	(set-dst '(r7 r6))
	(map-src-to-dst)
	(sdl-call sdl_SDL_CreateTextureFromSurface))

(defcompilefun sdl-destroy-texture (%1)
	;texture*
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_SDL_DestroyTexture))

(defcompilefun sdl-render-copy (%1 %2 %3 %4)
	;renderer**, texture*, srect*, drect*
	(set-src `(,%1 ,%2 ,%3 ,%4))
	(set-dst '(r7 r6 r2 r1))
	(map-src-to-dst)
	(sdl-call sdl_SDL_RenderCopy))

(defcompilefun sdl-set-texture-blend-mode (%1 %2)
	;texture*, blendmode
	(set-src `(,%1 ,%2))
	(set-dst '(r7 r6))
	(map-src-to-dst)
	(sdl-call sdl_SDL_SetTextureBlendMode))

(defcompilefun sdl-set-texture-color-mod (%1 %2 %3 %4)
	;texture*, byte r, byte g, byte b
	(set-src `(,%1 ,%2 ,%3 ,%4))
	(set-dst '(r7 r6 r2 r1))
	(map-src-to-dst)
	(sdl-call sdl_SDL_SetTextureColorMod))

(defcompilefun ttf_init ()
	(sdl-call sdl_TTF_Init))

(defcompilefun ttf_quit ()
	(sdl-call sdl_TTF_Quit))

(defcompilefun ttf-open-font (%1 %2)
	;name*, pointsize
	(set-src `(,%1 ,%2))
	(set-dst '(r7 r6))
	(map-src-to-dst)
	(sdl-call sdl_TTF_OpenFont))

(defcompilefun ttf-close-font (%1)
	;font*
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_TTF_CloseFont))

(defcompilefun ttf-size-utf8 (%1 %2 %3 %4)
	;font*, text*, int *w, int *h
	(set-src `(,%1 ,%2 ,%3 ,%4))
	(set-dst '(r7 r6 r2 r1))
	(map-src-to-dst)
	(sdl-call sdl_TTF_SizeUTF8))

(defcompilefun ttf-font-ascent (%1)
	;font*
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_TTF_FontAscent))

(defcompilefun ttf-font-descent (%1)
	;font*
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_TTF_FontDescent))

(defcompilefun ttf-font-height (%1)
	;font*
	(set-src `(,%1))
	(set-dst '(r7))
	(map-src-to-dst)
	(sdl-call sdl_TTF_FontHeight))

(defcompilefun ttf-render-utf8-blended (%1 %2 %3)
	;font*, text*, color
	(set-src `(,%1 ,%2 ,%3))
	(set-dst '(r7 r6 r2))
	(map-src-to-dst)
	(sdl-call sdl_TTF_RenderUTF8_Blended))
