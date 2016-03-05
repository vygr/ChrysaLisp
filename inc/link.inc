%ifndef LINK_1234
	%define LINK_1234

%include 'inc/list.inc'
%include 'inc/mail.inc'
%include 'inc/class.inc'

	def_class link
	def_method statics, sys/link_statics
	def_method init, sys/link_init
	def_method link, sys/link

;;;;;;;;;;;;;;;;;
; link structures
;;;;;;;;;;;;;;;;;

	lk_chan_status_ready	equ 0
	lk_chan_status_busy		equ 1

	struc lk_table
		lk_table_array:			resq	1
		lk_table_array_size:	resq	1
	endstruc

	struc lk_node
		lk_node_node:			resb	ln_node_size
		lk_node_table:			resb	lk_table_size
		lk_node_cpu_id:			resq	1
		lk_node_task_count:		resq	1
	endstruc

	struc lk_statics
		lk_statics_links_list:	resb	lh_list_size
	endstruc

	struc lk_chan
		lk_chan_status:		resq	1
		lk_chan_task_count:	resq	1
		lk_chan_msg:		resb	ml_msg_size
	endstruc

	struc lk_buffer
		lk_buffer_chan_1:	resb	lk_chan_size
		lk_buffer_chan_2:	resb	lk_chan_size
	endstruc

	struc lk_route
		lk_route_hops:		resq	1
	endstruc

%endif
