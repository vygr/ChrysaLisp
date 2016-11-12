(import 'inc/vp.inc)
(import 'inc/code.inc)
(import 'inc/gui.inc)
(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;
; sdl2 structures
;;;;;;;;;;;;;;;;;

%ifidn OS, Darwin
extern _SDL_SetMainReady
extern _SDL_Init
extern _SDL_Quit
extern _SDL_CreateWindow
extern _SDL_CreateWindowAndRenderer
extern _SDL_DestroyWindow
extern _SDL_Delay
extern _SDL_CreateRenderer
extern _SDL_SetRenderDrawColor
extern _SDL_RenderFillRect
extern _SDL_RenderPresent
extern _SDL_RenderSetClipRect
extern _SDL_SetRenderDrawBlendMode
extern _SDL_PumpEvents
extern _SDL_GetMouseState
extern _SDL_GetKeyboardState
extern _SDL_RenderDrawRect
extern _SDL_FreeSurface
extern _SDL_CreateTextureFromSurface
extern _SDL_DestroyTexture
extern _SDL_RenderCopy
extern _SDL_SetTextureBlendMode
extern _SDL_SetTextureColorMod

extern _TTF_Init
extern _TTF_Quit
extern _TTF_OpenFont
extern _TTF_CloseFont
extern _TTF_SizeUTF8
extern _TTF_FontAscent
extern _TTF_FontDescent
extern _TTF_FontHeight
extern _TTF_RenderUTF8_Blended
%elifidn OS, Linux
extern SDL_SetMainReady
extern SDL_Init
extern SDL_Quit
extern SDL_CreateWindow
extern SDL_CreateWindowAndRenderer
extern SDL_DestroyWindow
extern SDL_Delay
extern SDL_CreateRenderer
extern SDL_SetRenderDrawColor
extern SDL_RenderFillRect
extern SDL_RenderPresent
extern SDL_RenderSetClipRect
extern SDL_SetRenderDrawBlendMode
extern SDL_PumpEvents
extern SDL_GetMouseState
extern SDL_GetKeyboardState
extern SDL_RenderDrawRect
extern SDL_FreeSurface
extern SDL_CreateTextureFromSurface
extern SDL_DestroyTexture
extern SDL_RenderCopy
extern SDL_SetTextureBlendMode
extern SDL_SetTextureColorMod

extern TTF_Init
extern TTF_Quit
extern TTF_OpenFont
extern TTF_CloseFont
extern TTF_SizeUTF8
extern TTF_FontAscent
extern TTF_FontDescent
extern TTF_FontHeight
extern TTF_RenderUTF8_Blended
%endif
(def-struct 'sdl)
	byte sdl_SDL_SetMainReady
	byte sdl_SDL_Init
	byte sdl_SDL_Quit
	byte sdl_SDL_CreateWindow
	byte sdl_SDL_CreateWindowAndRenderer
	byte sdl_SDL_DestroyWindow
	byte sdl_SDL_Delay
	byte sdl_SDL_CreateRenderer
	byte sdl_SDL_SetRenderDrawColor
	byte sdl_SDL_RenderFillRect
	byte sdl_SDL_RenderPresent
	byte sdl_SDL_RenderSetClipRect
	byte sdl_SDL_SetRenderDrawBlendMode
	byte sdl_SDL_PumpEvents
	byte sdl_SDL_GetMouseState
	byte sdl_SDL_GetKeyboardState
	byte sdl_SDL_RenderDrawRect
	byte sdl_SDL_FreeSurface
	byte sdl_SDL_CreateTextureFromSurface
	byte sdl_SDL_DestroyTexture
	byte sdl_SDL_RenderCopy
	byte sdl_SDL_SetTextureBlendMode
	byte sdl_SDL_SetTextureColorMod

	byte sdl_TTF_Init
	byte sdl_TTF_Quit
	byte sdl_TTF_OpenFont
	byte sdl_TTF_CloseFont
	byte sdl_TTF_SizeUTF8
	byte sdl_TTF_FontAscent
	byte sdl_TTF_FontDescent
	byte sdl_TTF_FontHeight
	byte sdl_TTF_RenderUTF8_Blended
(def-struct-end)

(equate 'SDL_INIT_TIMER		 0x00000001)
(equate 'SDL_INIT_AUDIO		 0x00000010)
(equate 'SDL_INIT_VIDEO		 0x00000020)
(equate 'SDL_INIT_JOYSTICK	 0x00000200)
(equate 'SDL_INIT_HAPTIC		 0x00001000)
(equate 'SDL_INIT_GAMECONTROLLER 0x00002000)
(equate 'SDL_INIT_EVENTS		 0x00004000)
(equate 'SDL_INIT_NOPARACHUTE 0x00100000)
(equate 'SDL_INIT_EVERYTHING	 SDL_INIT_TIMER | SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_EVENTS | \)
							SDL_INIT_JOYSTICK | SDL_INIT_HAPTIC | SDL_INIT_GAMECONTROLLER

(equate 'SDL_WINDOW_FULLSCREEN		 0x00000001)
(equate 'SDL_WINDOW_OPENGL			 0x00000002)
(equate 'SDL_WINDOW_SHOWN			 0x00000004)
(equate 'SDL_WINDOW_HIDDEN			 0x00000008)
(equate 'SDL_WINDOW_BORDERLESS		 0x00000010)
(equate 'SDL_WINDOW_RESIZABLE		 0x00000020)
(equate 'SDL_WINDOW_MINIMIZED		 0x00000040)
(equate 'SDL_WINDOW_MAXIMIZED		 0x00000080)
(equate 'SDL_WINDOW_INPUT_GRABBED	 0x00000100)
(equate 'SDL_WINDOW_INPUT_FOCUS		 0x00000200)
(equate 'SDL_WINDOW_MOUSE_FOCUS		 0x00000400)
(equate 'SDL_WINDOW_FULLSCREEN_DESKTOP SDL_WINDOW_FULLSCREEN | 0x00001000)
(equate 'SDL_WINDOW_FOREIGN			 0x00000800)
(equate 'SDL_WINDOW_ALLOW_HIGHDPI	 0x00002000)

(equate 'SDL_WINDOWPOS_UNDEFINED_MASK			 0x1FFF0000)
%define	SDL_WINDOWPOS_UNDEFINED_DISPLAY(X)	(SDL_WINDOWPOS_UNDEFINED_MASK|(X))
%define	SDL_WINDOWPOS_UNDEFINED				SDL_WINDOWPOS_UNDEFINED_DISPLAY(0)
%define	SDL_WINDOWPOS_ISUNDEFINED(X)		(((X)&0xFFFF0000) == SDL_WINDOWPOS_UNDEFINED_MASK)

(equate 'SDL_WINDOWPOS_CENTERED_MASK				 0x2FFF0000)
%define	SDL_WINDOWPOS_CENTERED_DISPLAY(X)	(SDL_WINDOWPOS_CENTERED_MASK|(X))
%define	SDL_WINDOWPOS_CENTERED				SDL_WINDOWPOS_CENTERED_DISPLAY(0)
%define	SDL_WINDOWPOS_ISCENTERED(X)			(((X)&0xFFFF0000) == SDL_WINDOWPOS_CENTERED_MASK)

(equate 'SDL_RENDERER_SOFTWARE	 0x00000001)
(equate 'SDL_RENDERER_ACCELERATED 0x00000002)
(equate 'SDL_RENDERER_PRESENTVSYNC 0x00000004)
(equate 'SDL_RENDERER_TARGETTEXTURE 0x00000008)

(equate 'SDL_BLENDMODE_NONE	 0)
(equate 'SDL_BLENDMODE_BLEND	 1)
(equate 'SDL_BLENDMODE_ADD	 2)
(equate 'SDL_BLENDMODE_MOD	 3)

(def-struct 'sdl_rect)
	(int 'sdl_rect_x)
	(int 'sdl_rect_y)
	(int 'sdl_rect_w)
	(int 'sdl_rect_h)
(def-struct-end)

(def-struct 'sdl_surface)
	(int 'sdl_flags)
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

%macro sdl_call 1
	f_bind gui_gui, statics, r0
	vp_cpy [r0 + gui_statics_sdl_funcs], r0
	vp_call [r0 + (%1 * 8)]
%endmacro

%macro sdl_set_main_ready 0
	sdl_call sdl_SDL_SetMainReady
%endmacro

%macro sdl_init 1
	;flags
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_SDL_Init
%endmacro

%macro sdl_delay 1
	;time
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_SDL_Delay
%endmacro

%macro sdl_quit 0
	sdl_call sdl_SDL_Quit
%endmacro

%macro sdl_create_window 6
	;title, x, y, w, h, flags
	set_src %1, %2, %3, %4, %5, %6
	set_dst r7, r6, r2, r1, r8, r9
	map_src_to_dst
	sdl_call sdl_SDL_CreateWindow
%endmacro

%macro sdl_create_window_and_renderer 5
	;w, h, flags, window**, renderer**
	set_src %1, %2, %3, %4, %5
	set_dst r7, r6, r2, r1, r8
	map_src_to_dst
	sdl_call sdl_SDL_CreateWindowAndRenderer
%endmacro

%macro sdl_destroy_window 1
	;window
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_SDL_DestroyWindow
%endmacro

%macro sdl_create_renderer 3
	;window*, index, flags
	set_src %1, %2, %3
	set_dst r7, r6, r2
	map_src_to_dst
	sdl_call sdl_SDL_CreateRenderer
%endmacro

%macro sdl_set_render_draw_color 5
	;renderer**, r, g, b, a
	set_src %1, %2, %3, %4, %5
	set_dst r7, r6, r2, r1, r8
	map_src_to_dst
	sdl_call sdl_SDL_SetRenderDrawColor
%endmacro

%macro sdl_render_fill_rect 2
	;renderer**, rect*
	set_src %1, %2
	set_dst r7, r6
	map_src_to_dst
	sdl_call sdl_SDL_RenderFillRect
%endmacro

%macro sdl_render_draw_rect 2
	;renderer**, rect*
	set_src %1, %2
	set_dst r7, r6
	map_src_to_dst
	sdl_call sdl_SDL_RenderDrawRect
%endmacro

%macro sdl_render_present 1
	;renderer**
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_SDL_RenderPresent
%endmacro

%macro sdl_render_set_clip_rect 2
	;renderer**, rect*
	set_src %1, %2
	set_dst r7, r6
	map_src_to_dst
	sdl_call sdl_SDL_RenderSetClipRect
%endmacro

%macro sdl_set_render_draw_blend_mode 2
	;renderer**, mode
	set_src %1, %2
	set_dst r7, r6
	map_src_to_dst
	sdl_call sdl_SDL_SetRenderDrawBlendMode
%endmacro

%macro sdl_pump_events 0
	sdl_call sdl_SDL_PumpEvents
%endmacro

%macro sdl_get_mouse_state 2
	;x*, y*
	set_src %1, %2
	set_dst r7, r6
	map_src_to_dst
	sdl_call sdl_SDL_GetMouseState
%endmacro

%macro sdl_get_keyboard_state 1
	;*int
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_SDL_GetKeyboardState
%endmacro

%macro sdl_free_surface 1
	;surface*
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_SDL_FreeSurface
%endmacro

%macro sdl_create_texture_from_surface 2
	;renderer**, surface*
	set_src %1, %2
	set_dst r7, r6
	map_src_to_dst
	sdl_call sdl_SDL_CreateTextureFromSurface
%endmacro

%macro sdl_destroy_texture 1
	;texture*
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_SDL_DestroyTexture
%endmacro

%macro sdl_render_copy 4
	;renderer**, texture*, srect*, drect*
	set_src %1, %2, %3, %4
	set_dst r7, r6, r2, r1
	map_src_to_dst
	sdl_call sdl_SDL_RenderCopy
%endmacro

%macro sdl_set_texture_blend_mode 2
	;texture*, blendmode
	set_src %1, %2
	set_dst r7, r6
	map_src_to_dst
	sdl_call sdl_SDL_SetTextureBlendMode
%endmacro

%macro sdl_set_texture_color_mod 4
	;texture*, byte r, byte g, byte b
	set_src %1, %2, %3, %4
	set_dst r7, r6, r2, r1
	map_src_to_dst
	sdl_call sdl_SDL_SetTextureColorMod
%endmacro

%macro ttf_init 0
	sdl_call sdl_TTF_Init
%endmacro

%macro ttf_quit 0
	sdl_call sdl_TTF_Quit
%endmacro

%macro ttf_open_font 2
	;name*, pointsize
	set_src %1, %2
	set_dst r7, r6
	map_src_to_dst
	sdl_call sdl_TTF_OpenFont
%endmacro

%macro ttf_close_font 1
	;font*
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_TTF_CloseFont
%endmacro

%macro ttf_size_utf8 4
	;font*, text*, int *w, int *h
	set_src %1, %2, %3, %4
	set_dst r7, r6, r2, r1
	map_src_to_dst
	sdl_call sdl_TTF_SizeUTF8
%endmacro

%macro ttf_font_ascent 1
	;font*
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_TTF_FontAscent
%endmacro

%macro ttf_font_descent 1
	;font*
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_TTF_FontDescent
%endmacro

%macro ttf_font_height 1
	;font*
	set_src %1
	set_dst r7
	map_src_to_dst
	sdl_call sdl_TTF_FontHeight
%endmacro

%macro ttf_render_utf8_blended 3
	;font*, text*, color
	set_src %1, %2, %3
	set_dst r7, r6, r2
	map_src_to_dst
	sdl_call sdl_TTF_RenderUTF8_Blended
%endmacro
