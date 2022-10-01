(include "sys/list/class.inc")
(include "sys/mail/class.inc")
(include "class/str/class.inc")

(def-class sys_link :nil
	(dec-method :link sys/link/link)
	(dec-method :usb_link sys/link/usb_link)
	(dec-method :in sys/link/in)
	(dec-method :out sys/link/out))

;;;;;;;;;;;;;;;;;
; link structures
;;;;;;;;;;;;;;;;;

;msg data area max size when allocated as a string object
;allowing for heap pointer and null byte, rounded down for faster memcpy
(defcvar 'lk_data_size (- 1024 str_size +ptr_size +long_size) 'lk_page_size 4096)

(def-enum lk_chan_status 0
	(enum ready busy))

(def-struct lk_node 0
	(struct peer_node_id node_id_size)
	(uint task_count))

(def-struct lk_msg 0
	(uint status task_count)
	(struct peer_node_id node_id_size)
	(struct stamp (- msg_stamp_end msg_stamp_start))
	(struct data lk_data_size))

(def-struct lk_chan 0
	(struct msg0 lk_msg_size)
	(struct msgs (* (/ (- (- lk_page_size +long_size) base) lk_msg_size) lk_msg_size)))

(def-struct lk_buffer 0
	(struct chan_1 lk_chan_size)
	(ulong towel)
	(align lk_page_size)
	(struct chan_2 lk_chan_size))

;(print lk_data_size " " lk_page_size " " (/ lk_chan_size lk_msg_size))
