%ifndef LOAD_1234
	%define LOAD_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;
; load static class
;;;;;;;;;;;;;;;;;;;

	def_class load
	def_method statics, sys/load_statics
	def_method init, sys/load_init
	def_method deinit, sys/load_deinit
	def_method bind, sys/load_bind

;;;;;;;;;;;;;;;;;
; load structures
;;;;;;;;;;;;;;;;;

	ld_block_size	equ 16384
	ld_page_size	equ 4096
	ld_reloc_size	equ 1024

	struc ld_statics
		ld_statics_function_list:	resq 1
		ld_statics_block_list:		resq 1
		ld_statics_block_start:		resq 1
		ld_statics_block_end:		resq 1
		ld_statics_reloc_stack:		resq 1
		ld_statics_stat_buffer:		resb stat_size
		ld_statics_reloc_buffer:	resb ld_reloc_size
	endstruc

%endif
