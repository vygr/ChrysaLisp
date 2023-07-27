(include "././list/class.inc")
(include "././mail/class.inc")
(include "class/str/class.inc")
(include "sys/mem/class.inc")

(def-class sys_link :nil
	(dec-method :link sys/link/link)
	(dec-method :in sys/link/in)
	(dec-method :out sys/link/out))

;;;;;;;;;;;;;;;;;
; link structures
;;;;;;;;;;;;;;;;;

;msg data area max size when allocated as a string object
;allowing for heap pointer and null byte, rounded down for faster memcpy
(defq lk_page_size 4096)
(defcvar 'lk_data_size (- 1024 str_size +long_size sys_mem_header_size))

(def-enum lk_chan_status 0
	(enum ready busy))

(def-struct lk_node 0
	(struct peer_node_id node_id_size)
	(uint task_count))

(def-struct lk_buf 0
	(uint status hash task_count frag_length frag_offset total_length)
	(struct peer_node_id node_id_size)
	(struct dest net_id_size)
	(struct src net_id_size)
	(struct data lk_data_size))

(def-struct lk_chan 0
	(struct msg0 lk_buf_size)
	(struct msg1 lk_buf_size)
	(struct msg2 lk_buf_size))

(def-struct lk_shmem 0
	(struct chan_1 lk_chan_size)
	(ulong towel)
	(align lk_page_size)
	(struct chan_2 lk_chan_size)
	(align lk_page_size))

;; (print "page size: " lk_page_size)
;; (print "packet size: " lk_data_size)
;; (print "chan size: " lk_chan_size)
;; (print "buffer size: " lk_shmem_size)
;; (print "que size: " (/ lk_chan_size lk_buf_size))
