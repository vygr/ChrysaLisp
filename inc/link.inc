(import 'inc/class.inc)
(import 'inc/code.inc)
(import 'inc/list.inc)
(import 'inc/mail.inc)

(def-class 'sys_link)
(def-method 'statics 'sys/link_statics)
(def-method 'init 'sys/link_init)
(def-method 'link 'sys/link)

;;;;;;;;;;;;;;;;;
; link structures
;;;;;;;;;;;;;;;;;

(def-enum)
	(enum 'lk_chan_status_ready)
	(enum 'lk_chan_status_busy)
(def-enum-end)

(def-struct 'lk_table)
	(ptr 'lk_table_array)
	(ulong 'lk_table_array_size)
(def-struct-end)

(def-struct 'lk_node 'ln_node)
	(struct 'lk_node_table 'lk_table)
	(ulong 'lk_node_cpu_id)
	(ulong 'lk_node_task_count)
(def-struct-end)

(def-struct 'lk_statics)
	(struct 'lk_statics_links_list 'lh_list)
(def-struct-end)

(def-struct 'lk_chan)
	(ulong 'lk_chan_status)
	(ulong 'lk_chan_task_count)
	(struct 'lk_chan_msg 'msg)
(def-struct-end)

(def-struct 'lk_buffer)
	(struct 'lk_buffer_chan_1 'lk_chan)
	(struct 'lk_buffer_chan_2 'lk_chan)
(def-struct-end)

(def-struct 'lk_route)
	(ulong 'lk_route_hops)
(def-struct-end)
