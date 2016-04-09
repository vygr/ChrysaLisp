%ifndef LOAD_1234
%define LOAD_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;
; load static class
;;;;;;;;;;;;;;;;;;;

	def_class sys_load
	def_method statics, sys/load_statics
	def_method init, sys/load_init
	def_method deinit, sys/load_deinit
	def_method bind, sys/load_bind

;;;;;;;;;;;;;;;;;
; load structures
;;;;;;;;;;;;;;;;;

	ld_block_size	equ 16384
	ld_page_size	equ 4096
	ld_reloc_size	equ 2048

	def_structure	ld_statics
		def_long	ld_statics_function_list
		def_long	ld_statics_block_list
		def_long	ld_statics_block_start
		def_long	ld_statics_block_end
		def_long	ld_statics_reloc_stack
		def_struct	ld_statics_stat_buffer, stat
		def_struct	ld_statics_reloc_buffer, ld_reloc
	def_structure_end

%endif
