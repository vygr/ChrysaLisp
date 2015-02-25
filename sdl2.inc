%ifndef SDL2_1234
    %define SDL2_1234

%include "vp.inc"
%include "code.inc"

;;;;;;;;;;;;;;;;;
; sdl2 structures
;;;;;;;;;;;;;;;;;

	extern _SDL_SetMainReady
	extern _SDL_Init
	extern _SDL_Quit

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

;;;;;;;;;;;;;
; sdl2 macros
;;;;;;;;;;;;;

	%macro sdl_setmainready 0
		vp_call _SDL_SetMainReady
	%endmacro

	%macro sdl_init 1
		;flags
		vp_cpy %1, r7
		vp_call _SDL_Init
	%endmacro

	%macro sdl_quit 0
		vp_call _SDL_Quit
	%endmacro

%endif
