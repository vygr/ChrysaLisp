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

(equate 'ld_block_size (mul 64 1024))
(equate 'ld_page_size 4096)
(equate 'ld_reloc_size 3072)
(equate 'ld_name_size 120)

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
