(include "sys/task/class.inc")
(include "sys/mem/class.inc")
(include "gui/gui/class.inc")

;;;;;;;;;;;;;;;;;;;;;;
; system statics class
;;;;;;;;;;;;;;;;;;;;;;

(def-struct statics 0
	;sys_task statics
	(struct sys_task_kernel_node_id node_id_size)
	(ptr sys_task_current_tcb sys_task_kernel_tcb)
	(ulong sys_task_then)
	(uint sys_task_task_count)
	(align)
	;keep these together
	(struct sys_task_timer_list lh_list_size)
	(struct sys_task_task_lists (* num_task_lists lh_list_size))
	(align)

	;sys_mail statics
	(uint sys_mail_session sys_mail_next_mbox_id)
	(ptr sys_mail_links_array sys_mail_node_map sys_mail_out_mbox_addr)
	(struct sys_mail_msg_heap hp_heap_size)
	(struct sys_mail_mbox_heap hp_heap_size)
	;keep these together
	(struct sys_mail_timeout_list lh_list_size)
	(struct sys_mail_offchip_list lh_list_size)
	(struct sys_mail_parcel_list lh_list_size)
	(struct sys_mail_mbox_buckets (* num_mbox_buckets lh_list_size))
	(align)

	;sys_mem statics
	(struct sys_mem_obj_list lh_list_size)
	(long sys_mem_total)
	(struct sys_mem_heaps
		(progn
			(defq s mem_cell_min_size h 0)
			(while (<= s mem_cell_max_size)
				(setq h (+ h hp_heap_size) s (+ s s))) h))
	(align)

	;sys_load statics
	(ptr sys_load_host_os_funcs sys_load_host_gui_funcs
		sys_load_function_flist sys_load_block_flist
		sys_load_block_start sys_load_block_end
		sys_load_reloc_stack sys_load_reloc_buffer
		sys_load_stat_buffer sys_load_name_buffer)
	(align)

	;sys_math seed
	(long sys_math_seed)
	(align)

	;sym statics
	(ptr sym_intern sym_syms)
	(align)

	;num statics
	(ptr num_intern)
	(align)

	;font statics
	(ptr font_intern)
	(ulong font_count)
	(align)

	;gui statics
	(struct gui_rect_heap hp_heap_size)
	(ptr gui_window gui_screen gui_temps)
	(ulong gui_renderer gui_texture gui_next_id)
	(uint gui_dirty_flag gui_old_region_index gui_flags)
	(int gui_mouse_x gui_mouse_y)
	(align)
	(struct gui_old_regions (* num_old_regions +ptr_size))
	(align)

	;pixmap statics
	(ptr pixmap_intern)
	(align)

	;lisp statics
	(ptr lisp_boot_env)
	(align))
