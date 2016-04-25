%ifndef GUI_1234
%define GUI_1234

%include 'inc/list.inc'
%include 'inc/heap.inc'

	SCREEN_WIDTH	equ 1280
	SCREEN_HEIGHT	equ 960

;;;;;;;;;;;;;;;;;;;;;
; gui statics classes
;;;;;;;;;;;;;;;;;;;;;

	def_class gui_gui
	def_method statics, gui/gui_statics
	def_method init, gui/gui_init
	def_method deinit, gui/gui_deinit
	def_method update, gui/gui_update, static, {r0}
	def_method add, gui/gui_add, static, {r0}
	def_method gui, gui/gui

	def_class gui_region
	def_method translate, gui/region_translate, static, {r0, r8, r9}
	def_method clip_rect, gui/region_clip_rect, static, {r0, r1, r8, r9, r10, r11}
	def_method remove_rect, gui/region_remove_rect, static, {r0, r1, r8, r9, r10, r11}
	def_method cut_rect, gui/region_cut_rect, static, {r0, r1, r2, r8, r9, r10, r11}
	def_method copy_rect, gui/region_copy_rect, static, {r0, r1, r2, r8, r9, r10, r11}
	def_method paste_rect, gui/region_paste_rect, static, {r0, r1, r8, r9, r10, r11}
	def_method free, gui/region_free, static, {r0, r1}
	def_method copy_region, gui/region_copy_region, static, {r0, r1, r2, r3, r8, r9}
	def_method paste_region, gui/region_paste_region, static, {r0, r1, r2, r8, r9}
	def_method remove_region, gui/region_remove_region, static, {r0, r1, r2, r8, r9}

	def_class gui_ctx
	def_method box, gui/ctx_box, static, {r0, r8, r9, r10, r11}
	def_method filled_box, gui/ctx_filled_box, static, {r0, r8, r9, r10, r11}
	def_method blit, gui/ctx_blit, static, {r0, r1, r2, r8, r9, r10, r11}
	def_method set_color, gui/ctx_set_color, static, {r0, r1}

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

	def_structure	gui_statics
		def_long	gui_statics_sdl_funcs
		def_struct	gui_statics_rect_heap, hp_heap
		def_struct	gui_statics_sigslot_heap, hp_heap
		def_long	gui_statics_window
		def_long	gui_statics_screen
		def_long	gui_statics_renderer
		def_long	gui_statics_old_region
		def_long	gui_statics_x_pos
		def_long	gui_statics_y_pos
		def_long	gui_statics_buttons
		def_long	gui_statics_last_x_pos
		def_long	gui_statics_last_y_pos
		def_long	gui_statics_last_buttons
		def_long	gui_statics_last_view
	def_structure_end

	def_structure	gui_rect, ln_fnode
		def_long	gui_rect_x
		def_long	gui_rect_y
		def_long	gui_rect_x1
		def_long	gui_rect_y1
	def_structure_end

	def_structure	gui_ctx
		def_long	gui_ctx_sdl_ctx
		def_long	gui_ctx_dirty_region
		def_long	gui_ctx_x
		def_long	gui_ctx_y
	def_structure_end

	def_structure	gui_sigslot
		def_struct	gui_sigslot_sig_node, ln_node
		def_struct	gui_sigslot_slot_node, ln_node
		def_long	gui_sigslot_inst
		def_long	gui_sigslot_addr
	def_structure_end

;;;;;;;;;;;;;;;
; gui mail data
;;;;;;;;;;;;;;;

	def_structure	ev_type
		def_byte	ev_type_mouse
		def_byte	ev_type_key
	def_structure_end

	def_structure	ev_data, ml_msg_header
		def_long	ev_data_type
		def_long	ev_data_view
		def_long	ev_data_x
		def_long	ev_data_y
		def_long	ev_data_rx
		def_long	ev_data_ry
		def_long	ev_data_buttons
		def_long	ev_data_key
	def_structure_end

%endif
