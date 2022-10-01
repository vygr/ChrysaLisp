(include "sys/pii/class.inc")
(include "sys/pii/abi.inc")

;;;;;;;;;;;;;;;;;;;
; load static class
;;;;;;;;;;;;;;;;;;;

(def-class sys_load :nil
	(dec-method :init sys/load/init :static (abi-args 3) (:r0))
	(dec-method :bind sys/load/bind :static (:r0) (:r0))
	(dec-method :load sys/load/load :static (:r0) (:r0))
	(dec-method :find sys/load/find :static (:r0) (:r0 :r1))
	(dec-method :lisp_path sys/load/lisp_path :static (:r0 :r1) (:r0 :r1)))

;;;;;;;;;;;;;;;;;
; load structures
;;;;;;;;;;;;;;;;;

(defcvar 'ld_page_size 4096)
(defcvar 'ld_block_size (* ld_page_size 64))
(defcvar 'ld_reloc_buf_size (* ld_page_size 2))
(defcvar 'ld_name_size 128)
(defcvar 'ld_reloc_size (- ld_reloc_buf_size (align ld_name_size +long_size) (align stat_size +long_size)))
