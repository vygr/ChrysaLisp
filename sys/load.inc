(import 'sys/class.inc)
(import 'sys/code.inc)

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

(defcvar 'ld_page_size 4096)
(defcvar 'ld_block_size (mul 64 1024))
(defcvar 'ld_reloc_buf_size (mul ld_page_size 2))
(defcvar 'ld_name_size 128)
(defcvar 'ld_reloc_size (sub ld_reloc_buf_size ld_name_size stat_size))

(def-struct 'ld_statics)
	(ptr 'function_flist)
	(ptr 'block_flist)
	(ptr 'block_start)
	(ptr 'block_end)
	(ptr 'reloc_stack)
	(ptr 'reloc_buffer)
	(ptr 'stat_buffer)
	(ptr 'name_buffer)
(def-struct-end)
