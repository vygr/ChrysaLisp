%ifndef LOAD_1234
    %define LOAD_1234

%include "syscall.inc"

;;;;;;;;;;;;;;;;;
; load structures
;;;;;;;;;;;;;;;;;

	LD_BLOCK_SIZE	equ 16384
	LD_PAGE_SIZE	equ 4096

	struc LD_STATICS
		LD_STATICS_FUNCTION_LIST:	resq 1
		LD_STATICS_BLOCK_LIST:		resq 1
		LD_STATICS_BLOCK_START:		resq 1
		LD_STATICS_BLOCK_END:		resq 1
		LD_STATICS_STAT_BUFFER:		resb STAT_SIZE
		LD_STATICS_SIZE:
	endstruc

%endif
