(include 'sys/class.inc)
(include 'sys/abi.inc)

;;;;;;;;;;;;;;;;;;;
; load static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_load)
(dec-method 'statics 'sys/load/statics)
(dec-method 'init 'sys/load/init)
(dec-method 'bind 'sys/load/bind :static '(r0) '(r0))

;;;;;;;;;;;;;;;;;
; load structures
;;;;;;;;;;;;;;;;;

(defcvar 'ld_page_size 4096)
(defcvar 'ld_block_size (* ld_page_size 64))
(defcvar 'ld_reloc_buf_size (* ld_page_size 2))
(defcvar 'ld_name_size 128)
(defcvar 'ld_reloc_size (- ld_reloc_buf_size (align ld_name_size long_size) (align stat_size long_size)))

(def-struct 'ld_statics)
	(ptr 'host_funcs 'function_flist 'block_flist 'block_start 'block_end
		'reloc_stack 'reloc_buffer 'stat_buffer 'name_buffer)
(def-struct-end)
