%ifndef LINK_1234
%define LINK_1234

%include 'inc/list.inc'
%include 'inc/mail.inc'
%include 'inc/class.inc'

	def_class sys_link
	def_method statics, sys/link_statics
	def_method init, sys/link_init
	def_method link, sys/link

;;;;;;;;;;;;;;;;;
; link structures
;;;;;;;;;;;;;;;;;

	lk_chan_status_ready	equ 0
	lk_chan_status_busy		equ 1

	def_structure lk_table
		long lk_table_array
		long lk_table_array_size
	def_structure_end

	def_structure lk_node, ln_node
		struct lk_node_table, lk_table
		long lk_node_cpu_id
		long lk_node_task_count
	def_structure_end

	def_structure lk_statics
		struct lk_statics_links_list, lh_list
	def_structure_end

	def_structure lk_chan
		long lk_chan_status
		long lk_chan_task_count
		struct lk_chan_msg, ml_msg
	def_structure_end

	def_structure lk_buffer
		struct lk_buffer_chan_1, lk_chan
		struct lk_buffer_chan_2, lk_chan
	def_structure_end

	def_structure lk_route
		long lk_route_hops
	def_structure_end

%endif
