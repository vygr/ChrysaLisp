%use altreg

%ifidn OS, Darwin
	extern _SDL_SetMainReady
	extern _SDL_Init
	extern _SDL_GetError
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
	extern _SDL_CreateRGBSurfaceFrom

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
	extern SDL_GetError
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
	extern SDL_CreateRGBSurfaceFrom

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

;;;;;;;;;;;;;
; entry point
;;;;;;;;;;;;;

	fn_header_entry equ 12

	SECTION .text

	global main
main:
	;called by sdl !!!!!!!
	mov r0, r6
	lea r1, [rel sdl_func_table]

	;jump to boot image
	lea r3, [rel boot_image]
	mov edx, dword [r3 + fn_header_entry]
	add r3, r2
	jmp r3

;;;;;;;;;;;;;;;;;;;;
; prebound functions
;;;;;;;;;;;;;;;;;;;;

	align 8, db 0

boot_image:
	incbin 'obj/sys/boot_image'

	SECTION .data

	align 8, db 0
sdl_func_table:
%ifidn OS, Darwin
	dq _SDL_SetMainReady
	dq _SDL_Init
	dq _SDL_GetError
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
	dq _SDL_GetKeyboardState
	dq _SDL_RenderDrawRect
	dq _SDL_FreeSurface
	dq _SDL_CreateTextureFromSurface
	dq _SDL_DestroyTexture
	dq _SDL_RenderCopy
	dq _SDL_SetTextureBlendMode
	dq _SDL_SetTextureColorMod
	dq _SDL_CreateRGBSurfaceFrom

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
	dq SDL_GetError
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
	dq SDL_GetKeyboardState
	dq SDL_RenderDrawRect
	dq SDL_FreeSurface
	dq SDL_CreateTextureFromSurface
	dq SDL_DestroyTexture
	dq SDL_RenderCopy
	dq SDL_SetTextureBlendMode
	dq SDL_SetTextureColorMod
	dq SDL_CreateRGBSurfaceFrom

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
