(include "././list/class.inc")
(include "././mail/class.inc")
(include "class/str/class.inc")
(include "sys/mem/class.inc")

(def-class :sys_link :nil
	(dec-method :link sys/link/link)
	(dec-method :in sys/link/in)
	(dec-method :out sys/link/out))

;;;;;;;;;;;;;;;;;
; link structures
;;;;;;;;;;;;;;;;;

;msg data area max size when allocated as a string object
;allowing for heap pointer and null byte, rounded down for faster memcpy
(defq lk_page_size 4096 lk_sleep_min 100 lk_sleep_max 800
	lk_data_size (- lk_page_size +str_size +long_size +sys_mem_header_size)
	lk_bufs_per_chan 6)

(enums lk_chan_status 0
	(enum ready ping frag skip))

;buffer transfer unit
(structure lk_buf 0
	(uint status length)
	(align))

;peer info
(structure lk_node 0
	(nodeid peer_node_id)
	(uint task_count)
	(align))

;fragment info
(structure lk_frag 0
	(netid dest src)
	(uint length offset total)
	(align))

(structure lk_chan 0
	(struct data (* (+ lk_buf_size lk_frag_size lk_data_size) lk_bufs_per_chan)))

(structure lk_shmem 0
	(struct chan_1 lk_chan_size)
	; Negotiation "Towel" embedded in the alignment padding of chan_1
	(nodeid host_a)
	(nodeid host_b)
	(align lk_page_size)
	(struct chan_2 lk_chan_size))
