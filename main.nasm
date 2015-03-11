%include "vp.inc"
%include "load.inc"
%include "syscall.inc"

;;;;;;;;;;;;;
; entry point
;;;;;;;;;;;;;

	SECTION .text

	global _main
_main:
	;set prebound functions as read/write/executable
	vp_lea [rel ld_prebound], r0
	vp_and -LD_PAGE_SIZE, r0
	vp_lea [rel ld_prebounde], r1
	vp_sub r0, r1
	sys_mprotect r0, r1, PROT_READ|PROT_WRITE|PROT_EXEC

	;init loader
	vp_call ld_load_init_loader + 0x38

	;jump to kernel task
	vp_jmp ld_kernel + 0x30

;;;;;;;;;;;;;;;;;;;;
; prebound functions
;;;;;;;;;;;;;;;;;;;;

	align 8, db 0
ld_prebound:

ld_load_init_loader:
	incbin	'sys/load_init_loader'		;must be first function !
	incbin	'sys/load_function_load'	;must be second function !
	incbin	'sys/load_statics'			;must be third function !
	incbin	'sys/load_deinit_loader'	;must be included !
ld_kernel:
	incbin	'sys/kernel'				;must be included !

ld_prebounde:
	dq 0
