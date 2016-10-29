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

def_enum
	enum lk_chan_status_ready
	enum lk_chan_status_busy
def_enum_end

def_struct lk_table
	ptr lk_table_array
	ulong lk_table_array_size
def_struct_end

def_struct lk_node, ln_node
	struct lk_node_table, lk_table
	ulong lk_node_cpu_id
	ulong lk_node_task_count
def_struct_end

def_struct lk_statics
	struct lk_statics_links_list, lh_list
def_struct_end

def_struct lk_chan
	ulong lk_chan_status
	ulong lk_chan_task_count
	struct lk_chan_msg, msg
def_struct_end

def_struct lk_buffer
	struct lk_buffer_chan_1, lk_chan
	struct lk_buffer_chan_2, lk_chan
def_struct_end

def_struct lk_route
	ulong lk_route_hops
def_struct_end

%endif
