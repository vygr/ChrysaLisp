%ifndef SDL2_1234
%define SDL2_1234

%include 'inc/vp.inc'
%include 'inc/code.inc'
%include 'inc/gui.inc'

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
	def_structure	sdl
		def_byte	SDL_SetMainReady
		def_byte	SDL_Init
		def_byte	SDL_Quit
		def_byte	SDL_CreateWindow
		def_byte	SDL_CreateWindowAndRenderer
		def_byte	SDL_DestroyWindow
		def_byte	SDL_Delay
		def_byte	SDL_CreateRenderer
		def_byte	SDL_SetRenderDrawColor
		def_byte	SDL_RenderFillRect
		def_byte	SDL_RenderPresent
		def_byte	SDL_RenderSetClipRect
		def_byte	SDL_SetRenderDrawBlendMode
		def_byte	SDL_PUMPEVENTS
		def_byte	SDL_GetMouseState
		def_byte	SDL_RenderDrawRect
		def_byte	SDL_FreeSurface
		def_byte	SDL_CreateTextureFromSurface
		def_byte	SDL_DestroyTexture
		def_byte	SDL_RenderCopy
		def_byte	SDL_SetTextureBlendMode
		def_byte	SDL_SetTextureColorMod

		def_byte	TTF_Init
		def_byte	TTF_Quit
		def_byte	TTF_OpenFont
		def_byte	TTF_CloseFont
		def_byte	TTF_SizeUTF8
		def_byte	TTF_FontAscent
		def_byte	TTF_FontDescent
		def_byte	TTF_RenderUTF8_Blended
	def_structure_end

	SDL_INIT_TIMER			equ 0x00000001
	SDL_INIT_AUDIO			equ 0x00000010
	SDL_INIT_VIDEO			equ 0x00000020
	SDL_INIT_JOYSTICK		equ 0x00000200
	SDL_INIT_HAPTIC			equ 0x00001000
	SDL_INIT_GAMECONTROLLER	equ 0x00002000
	SDL_INIT_EVENTS			equ 0x00004000
	SDL_INIT_NOPARACHUTE	equ 0x00100000
	SDL_INIT_EVERYTHING		equ SDL_INIT_TIMER | SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_EVENTS | \
								SDL_INIT_JOYSTICK | SDL_INIT_HAPTIC | SDL_INIT_GAMECONTROLLER

	SDL_WINDOW_FULLSCREEN			equ 0x00000001
	SDL_WINDOW_OPENGL				equ 0x00000002
	SDL_WINDOW_SHOWN				equ 0x00000004
	SDL_WINDOW_HIDDEN				equ 0x00000008
	SDL_WINDOW_BORDERLESS			equ 0x00000010
	SDL_WINDOW_RESIZABLE			equ 0x00000020
	SDL_WINDOW_MINIMIZED			equ 0x00000040
	SDL_WINDOW_MAXIMIZED			equ 0x00000080
	SDL_WINDOW_INPUT_GRABBED		equ 0x00000100
	SDL_WINDOW_INPUT_FOCUS			equ 0x00000200
	SDL_WINDOW_MOUSE_FOCUS			equ 0x00000400
	SDL_WINDOW_FULLSCREEN_DESKTOP	equ SDL_WINDOW_FULLSCREEN | 0x00001000
	SDL_WINDOW_FOREIGN				equ 0x00000800
	SDL_WINDOW_ALLOW_HIGHDPI		equ 0x00002000

	SDL_WINDOWPOS_UNDEFINED_MASK				equ 0x1FFF0000
	%define	SDL_WINDOWPOS_UNDEFINED_DISPLAY(X)	(SDL_WINDOWPOS_UNDEFINED_MASK|(X))
	%define	SDL_WINDOWPOS_UNDEFINED				SDL_WINDOWPOS_UNDEFINED_DISPLAY(0)
	%define	SDL_WINDOWPOS_ISUNDEFINED(X)		(((X)&0xFFFF0000) == SDL_WINDOWPOS_UNDEFINED_MASK)

	SDL_WINDOWPOS_CENTERED_MASK					equ 0x2FFF0000
	%define	SDL_WINDOWPOS_CENTERED_DISPLAY(X)	(SDL_WINDOWPOS_CENTERED_MASK|(X))
	%define	SDL_WINDOWPOS_CENTERED				SDL_WINDOWPOS_CENTERED_DISPLAY(0)
	%define	SDL_WINDOWPOS_ISCENTERED(X)			(((X)&0xFFFF0000) == SDL_WINDOWPOS_CENTERED_MASK)

	SDL_RENDERER_SOFTWARE		equ 0x00000001
	SDL_RENDERER_ACCELERATED	equ 0x00000002
	SDL_RENDERER_PRESENTVSYNC	equ 0x00000004
	SDL_RENDERER_TARGETTEXTURE	equ 0x00000008

	SDL_BLENDMODE_NONE		equ 0
	SDL_BLENDMODE_BLEND		equ 1
	SDL_BLENDMODE_ADD		equ 2
	SDL_BLENDMODE_MOD		equ 3

	def_structure	sdl_rect
		def_int	sdl_rect_x
		def_int	sdl_rect_y
		def_int	sdl_rect_w
		def_int	sdl_rect_h
	def_structure_end

;;;;;;;;;;;;;
; sdl2 macros
;;;;;;;;;;;;;

	%macro sdl_set_main_ready 0
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_SetMainReady * 8)]
	%endmacro

	%macro sdl_init 1
		;flags
		vp_cpy %1, r7
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_Init * 8)]
	%endmacro

	%macro sdl_delay 1
		;time
		vp_cpy %1, r7
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_Delay * 8)]
	%endmacro

	%macro sdl_quit 0
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_Quit * 8)]
	%endmacro

	%macro sdl_create_window 6
		;title, x, y, w, h, flags
		vp_cpy %1, r7		;title
		vp_cpy %2, r6		;x
		vp_cpy %3, r2		;y
		vp_cpy %4, r1		;w
		vp_cpy %5, r8		;h
		vp_cpy %6, r9		;flags
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_CreateWindow * 8)]
	%endmacro

	%macro sdl_create_window_and_renderer 5
		;w, h, flags, window**, renderer**
		vp_cpy %1, r7		;w
		vp_cpy %2, r6		;h
		vp_cpy %3, r2		;flags
		vp_cpy %4, r1		;window**
		vp_cpy %5, r8		;renderer**
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_CreateWindowAndRenderer * 8)]
	%endmacro

	%macro sdl_destroy_window 1
		;window
		vp_cpy %1, r7
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_DestroyWindow * 8)]
	%endmacro

	%macro sdl_create_renderer 3
		;window*, index, flags
		vp_cpy %1, r7		;window*
		vp_cpy %2, r6		;index
		vp_cpy %3, r2		;flags
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_CreateRenderer * 8)]
	%endmacro

	%macro sdl_set_render_draw_color 5
		;renderer**, r, g, b, a
		vp_cpy %1, r7		;renderer**
		vp_cpy %2, r6		;r
		vp_cpy %3, r2		;g
		vp_cpy %4, r1		;b
		vp_cpy %5, r8		;a
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_SetRenderDrawColor * 8)]
	%endmacro

	%macro sdl_render_fill_rect 2
		;renderer**, rect*
		vp_cpy %1, r7		;renderer**
		vp_cpy %2, r6		;rect*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_RenderFillRect * 8)]
	%endmacro

	%macro sdl_render_draw_rect 2
		;renderer**, rect*
		vp_cpy %1, r7		;renderer**
		vp_cpy %2, r6		;rect*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_RenderDrawRect * 8)]
	%endmacro

	%macro sdl_render_present 1
		;renderer**
		vp_cpy %1, r7		;renderer**
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_RenderPresent * 8)]
	%endmacro

	%macro sdl_render_set_clip_rect 2
		;renderer**, rect*
		vp_cpy %1, r7		;renderer**
		vp_cpy %2, r6		;rect*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_RenderSetClipRect * 8)]
	%endmacro

	%macro sdl_set_render_draw_blend_mode 2
		;renderer**, mode
		vp_cpy %1, r7		;renderer**
		vp_cpy %2, r6		;mode
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_SetRenderDrawBlendMode * 8)]
	%endmacro

	%macro sdl_pump_events 0
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_PUMPEVENTS * 8)]
	%endmacro

	%macro sdl_get_mouse_state 2
		;x*, y*
		vp_cpy %1, r7		;x*
		vp_cpy %2, r6		;y*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_GetMouseState * 8)]
	%endmacro

	%macro sdl_free_surface 1
		;surface*
		vp_cpy %1, r7		;surface*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_FreeSurface * 8)]
	%endmacro

	%macro sdl_create_texture_from_surface 2
		;renderer**, surface*
		vp_cpy %1, r7		;renderer**
		vp_cpy %2, r6		;surface*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_CreateTextureFromSurface * 8)]
	%endmacro

	%macro sdl_destroy_texture 1
		;texture*
		vp_cpy %1, r7		;texture*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_DestroyTexture * 8)]
	%endmacro

	%macro sdl_render_copy 2
		;renderer**, texture*, srect*, drect*
		vp_cpy %1, r7		;renderer**
		vp_cpy %2, r6		;texture*
		vp_cpy %3, r2		;srect*
		vp_cpy %4, r1		;drect*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_RenderCopy * 8)]
	%endmacro

	%macro sdl_set_texture_blend_mode 2
		;texture*, blendmode
		vp_cpy %1, r7		;texture*
		vp_cpy %2, r6		;blendmode
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_SetTextureBlendMode * 8)]
	%endmacro

	%macro sdl_set_texture_color_mod 4
		;texture*, byte r, byte g, byte b
		vp_cpy %1, r7		;texture*
		vp_cpy %2, r6		;r
		vp_cpy %3, r2		;g
		vp_cpy %4, r1		;b
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (SDL_SetTextureColorMod * 8)]
	%endmacro

	%macro ttf_init 0
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_Init * 8)]
	%endmacro

	%macro ttf_quit 0
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_Quit * 8)]
	%endmacro

	%macro ttf_open_font 2
		;name*, pointsize
		vp_cpy %1, r7		;name*
		vp_cpy %2, r6		;pointsize
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_OpenFont * 8)]
	%endmacro

	%macro ttf_close_font 1
		;font*
		vp_cpy %1, r7		;font*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_CloseFont * 8)]
	%endmacro

	%macro ttf_size_utf8 4
		;font*, text*, int *w, int *h
		vp_cpy %1, r7		;font*
		vp_cpy %2, r6		;text*
		vp_cpy %3, r2		;w*
		vp_cpy %4, r1		;h*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_SizeUTF8 * 8)]
	%endmacro

	%macro ttf_font_ascent 1
		;font*
		vp_cpy %1, r7		;font*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_FontAscent * 8)]
	%endmacro

	%macro ttf_font_descent 1
		;font*
		vp_cpy %1, r7		;font*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_FontDescent * 8)]
	%endmacro

	%macro ttf_font_height 1
		;font*
		vp_cpy %1, r7		;font*
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_FontHeight * 8)]
	%endmacro

	%macro ttf_render_utf8_blended 3
		;font*, text*, color
		vp_cpy %1, r7		;font*
		vp_cpy %2, r6		;text*
		vp_cpy %3, r2		;color
		static_bind gui, statics, r0
		vp_cpy [r0 + gui_statics_sdl_funcs], r0
		vp_call [r0 + (TTF_RenderUTF8_Blended * 8)]
	%endmacro

%endif
