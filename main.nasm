%include 'inc/func.inc'
%include 'inc/sdl2.inc'

;;;;;;;;;;;;;
; entry point
;;;;;;;;;;;;;

	SECTION .text

	global main
main:
	;called by sdl !!!!!!!
	vp_push r6

	;init loader and prebind
	vp_lea [rel ld_load_init_loader], r1
	vp_add [r1 + fn_header_entry], r1
	vp_call r1

	;init gui
	vp_lea [rel sdl_func_table], r0
	vp_lea [rel ld_gui_init_gui], r1
	vp_add [r1 + fn_header_entry], r1
	vp_call r1

	;jump to kernel task
	vp_pop r0
	vp_lea [rel ld_kernel], r1
	vp_add [r1 + fn_header_entry], r1
	vp_jmp r1

;;;;;;;;;;;;;;;;;;;;
; prebound functions
;;;;;;;;;;;;;;;;;;;;

	align 8, db 0

ld_load_init_loader:
	incbin	'sys/load_init'		;must be first function !
	incbin	'sys/load_bind'		;must be second function !
	incbin	'sys/load_statics'	;must be third function !
	incbin	'sys/load_deinit'	;must be included ! Because it unmaps all function blocks
ld_gui_init_gui:
	incbin	'gui/gui_init'		;must be included !
ld_kernel:
	incbin	'sys/kernel'		;must be included !
	dq 0, 0						;must mark end

	SECTION .data

	align 8, db 0
sdl_func_table:
%ifidn OS, Darwin
	dq _SDL_SetMainReady
	dq _SDL_Init
	dq _SDL_Quit
	dq _SDL_CreateWindow
	dq _SDL_CreateWindowAndRenderer
	dq _SDL_DestroyWindow
	dq _SDL_Delay
	dq _SDL_CreateRenderer
	dq _SDL_SetRenderDrawColor
	dq _SDL_RenderFillRect
	dq _SDL_RenderPresent
	dq _SDL_RenderSetClipRect
	dq _SDL_SetRenderDrawBlendMode
	dq _SDL_PumpEvents
	dq _SDL_GetMouseState
	dq _SDL_RenderDrawRect

	dq _TTF_Init
	dq _TTF_Quit
	dq _TTF_OpenFont
	dq _TTF_CloseFont
	dq _TTF_SizeUTF8
	dq _TTF_FontAscent
	dq _TTF_FontDescent
	dq _TTF_FontHeight
	dq _TTF_RenderUTF8_Blended
%elifidn OS, Linux
	dq SDL_SetMainReady
	dq SDL_Init
	dq SDL_Quit
	dq SDL_CreateWindow
	dq SDL_CreateWindowAndRenderer
	dq SDL_DestroyWindow
	dq SDL_Delay
	dq SDL_CreateRenderer
	dq SDL_SetRenderDrawColor
	dq SDL_RenderFillRect
	dq SDL_RenderPresent
	dq SDL_RenderSetClipRect
	dq SDL_SetRenderDrawBlendMode
	dq SDL_PumpEvents
	dq SDL_GetMouseState
	dq SDL_RenderDrawRect

	dq TTF_Init
	dq TTF_Quit
	dq TTF_OpenFont
	dq TTF_CloseFont
	dq TTF_SizeUTF8
	dq TTF_FontAscent
	dq TTF_FontDescent
	dq TTF_FontHeight
	dq TTF_RenderUTF8_Blended
%endif
