%ifndef LOAD_1234
	%define LOAD_1234

;;;;;;;;;;;;;;;;;
; load structures
;;;;;;;;;;;;;;;;;

	ld_block_size	equ 16384
	ld_page_size	equ 4096

	struc ld_statics
		ld_statics_function_list:	resq 1
		ld_statics_block_list:		resq 1
		ld_statics_block_start:		resq 1
		ld_statics_block_end:		resq 1
		ld_statics_stat_buffer:		resb stat_size
	endstruc

%endif
