(include "././pii/class.inc")

;;;;;;;;;;;;;;;;;;;
; load static class
;;;;;;;;;;;;;;;;;;;

(def-class :sys_load :nil
	(dec-method :init sys/load/init :static (:r0 :r1) (:r0) (:r0-:r14 :f0-:f15))
	(dec-method :bind sys/load/bind :static (:r0) (:r0) (:r0-:r7 :f0-:f15))
	(dec-method :load sys/load/load :static (:r0) (:r0) (:r0-:r7 :f0-:f15))
	(dec-method :find sys/load/find :static (:r0) (:r0 :r1) (:r0-:r2))
	(dec-method :search sys/load/search :static (:r6) (:r0) (:r0-:r5 :r7))
	(dec-method :place sys/load/place :static (:r0) (:r0) (:r0-:r7 :f0-:f15))
	(dec-method :server sys/load/server :static (:r0) (:r0) (:r0-:r3 :r5-:r7 :f0-:f15))
	(dec-method :sweep sys/load/sweep :static :nil :nil (:r0-:r2 :r5-:r13 :f0-:f15))
	(dec-method :lisp_path sys/load/lisp_path :static (:r0 :r1) (:r0 :r1) (:r1-:r5 :f0-:f15))
	(dec-method :lisp_server sys/load/lisp_server :static (:r0 :r1) (:r0 :r1) (:r1-:r7 :f0-:f15))
	(dec-method :lisp_place sys/load/lisp_place :static (:r0 :r1) (:r0 :r1) (:r1-:r7 :f0-:f15)))

;;;;;;;;;;;;;;;;;
; load structures
;;;;;;;;;;;;;;;;;

(defcvar 'ld_page_size 4096)
(defcvar 'ld_block_size (* ld_page_size 64))
(defcvar 'ld_reloc_buf_size (* ld_page_size 2))
(defcvar 'ld_name_size 128)
(defcvar 'ld_reloc_size (- ld_reloc_buf_size (align ld_name_size +long_size) (align +stat_size +long_size)))
(defcvar 'ld_prefix_size 64)

;a function server: the functions a node has of another system, bound by
;the tasks of it that run here, in blocks of their own. Its list, blocks
;and where the next function goes are in the order the statics have the
;node's own, so the node's own are a server too, at statics_sys_load_function_flist
;less ld_server_flist. Its folder under the load path, source/<id>/, has
;what it has been given, translated for this CPU. It goes when no task of
;the node has it, (:sys_load :sweep)
(structure +ld_server +ln_fnode_size
	(ptr flist blocks start end)
	(long mark)
	(struct name ld_prefix_size))
