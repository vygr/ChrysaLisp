(include "sys/task/class.inc")
(include "sys/mem/class.inc")
(include "gui/gui/class.inc")

;;;;;;;;;;;;;;;;;;;;;;
; system statics class
;;;;;;;;;;;;;;;;;;;;;;

(def-struct 'statics)
	;sys_task statics
	(struct 'sys_task_kernel_node_id 'node_id)
	(ptr 'sys_task_current_tcb 'sys_task_kernel_tcb)
	(uint 'sys_task_task_count)
	(local-align)
	;keep these together
	(struct 'sys_task_timer_list 'lh_list)
	(struct 'sys_task_task_lists (* num_task_lists lh_list_size))
	(local-align)

	;sys_mail statics
	(uint 'sys_mail_session 'sys_mail_next_mbox_id)
	(ptr 'sys_mail_links_array 'sys_mail_node_map 'sys_mail_out_mbox_addr)
	(struct 'sys_mail_msg_heap 'hp_heap)
	(struct 'sys_mail_mbox_heap 'hp_heap)
	;keep these together
	(struct 'sys_mail_timeout_list 'lh_list)
	(struct 'sys_mail_offchip_list 'lh_list)
	(struct 'sys_mail_parcel_list 'lh_list)
	(struct 'sys_mail_mbox_buckets (* num_mbox_buckets lh_list_size))
	(local-align)

	;sys_mem statics
	(struct 'sys_mem_obj_list 'lh_list)
	(long 'sys_mem_total)
	(offset 'sys_mem_heaps)
	(let ((s mem_cell_min_size))
		(while (<= s mem_cell_max_size)
			(setq *struct_sp* (+ *struct_sp* hp_heap_size))
			(setq s (+ s s))))
	(local-align)

	;sys_load statics
	(ptr 'sys_load_host_os_funcs 'sys_load_host_gui_funcs
		'sys_load_function_flist 'sys_load_block_flist
		'sys_load_block_start 'sys_load_block_end
		'sys_load_reloc_stack 'sys_load_reloc_buffer
		'sys_load_stat_buffer 'sys_load_name_buffer)
	(local-align)

	;sys_math seed
	(long 'sys_math_seed)
	(local-align)

	;sym statics
	(ptr 'sym_intern 'sym_syms)
	(local-align)

	;font statics
	(ptr 'font_intern)
	(ulong 'font_count)
	(local-align)

	;gui statics
	(struct 'gui_rect_heap 'hp_heap)
	(ptr 'gui_window 'gui_screen 'gui_temps)
	(ulong 'gui_renderer 'gui_texture 'gui_next_id)
	(uint 'gui_dirty_flag 'gui_old_region_index)
	(int 'gui_mouse_x 'gui_mouse_y)
	(struct 'gui_old_regions (* num_old_regions +ptr_size+))
	(local-align)

	;pixmap statics
	(ptr 'pixmap_intern)
	(local-align)

	;lisp statics
	(ptr 'lisp_boot_env)
	(local-align)

(def-struct-end)
