;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nasm -f macho64 gui.nasm
;; ld -macosx_version_min 10.6 -o gui -e _main gui.o
;; ./gui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "vp.inc"
%include "code.inc"
%include "syscall.inc"
%include "sdl2.inc"

;;;;;;;;;;;;;
; entry point
;;;;;;;;;;;;;

	SECTION .text

	global _main
_main:
	;init sdl2
	sdl_setmainready
	sdl_init SDL_INIT_VIDEO

	;deinit sdl2
	sdl_quit

	;exit
	sys_exit 0
