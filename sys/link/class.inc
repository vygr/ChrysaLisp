(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'sys/list/class.inc)
(import 'sys/mail/class.inc)
(import 'class/string/class.inc)

(def-class 'sys_link)
(dec-method 'init 'sys/link/init)
(dec-method 'link 'sys/link/link)

;;;;;;;;;;;;;;;;;
; link structures
;;;;;;;;;;;;;;;;;

(defcvar 'lk_data_size (sub 512 string_size ptr_size long_size))

(def-enum 'lk_chan_status)
	(enum 'ready 'busy)
(def-enum-end)

(def-struct 'lk_table)
	(pptr 'array)
	(ulong 'array_size)
(def-struct-end)

(def-struct 'lk_node)
	(struct 'table 'lk_table)
	(uint 'cpu_id)
	(uint 'task_count)
(def-struct-end)

(def-struct 'lk_msg)
	(uint 'status)
	(local-align)
	(struct 'stamp msg_stamp_size)
	(struct 'data lk_data_size)
(def-struct-end)

(def-struct 'lk_chan)
	(uint 'task_count)
	(local-align)
	(struct 'msg0 'lk_msg)
	(struct 'msg1 'lk_msg)
	(struct 'msg2 'lk_msg)
	(struct 'msg3 'lk_msg)
	(struct 'msg4 'lk_msg)
	(struct 'msg5 'lk_msg)
(def-struct-end)

(def-struct 'lk_buffer)
	(struct 'chan_1 'lk_chan)
	(struct 'chan_2 'lk_chan)
(def-struct-end)

(def-struct 'lk_route)
	(uint 'hops)
(def-struct-end)
