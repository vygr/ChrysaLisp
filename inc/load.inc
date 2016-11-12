%ifndef LOAD_1234
%define LOAD_1234

(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;;;
; load static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_load)
(def-method 'statics 'sys/load_statics)
(def-method 'init 'sys/load_init)
(def-method 'deinit 'sys/load_deinit)
(def-method 'bind 'sys/load_bind 'static '(r0) '(r0))

;;;;;;;;;;;;;;;;;
; load structures
;;;;;;;;;;;;;;;;;

ld_block_size	equ 64*1024
ld_page_size	equ 4096
ld_reloc_size	equ 3072
ld_name_size	equ 120

(def-struct 'ld_statics)
	(ptr 'ld_statics_function_flist)
	(ptr 'ld_statics_block_flist)
	(ptr 'ld_statics_block_start)
	(ptr 'ld_statics_block_end)
	(ptr 'ld_statics_reloc_stack)
	(struct 'ld_statics_stat_buffer 'stat)
	(struct 'ld_statics_reloc_buffer 'ld_reloc)
	(struct 'ld_statics_name_buffer 'ld_name)
(def-struct-end)

%endif
