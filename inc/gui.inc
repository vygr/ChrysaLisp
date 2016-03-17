%ifndef GUI_1234
%define GUI_1234

%include 'inc/list.inc'
%include 'inc/heap.inc'

;;;;;;;;;;;;;;;;;;;;;
; gui statics classes
;;;;;;;;;;;;;;;;;;;;;

	def_class gui
	def_method statics, gui/gui_statics
	def_method init, gui/gui_init
	def_method deinit, gui/gui_deinit
	def_method draw, gui/gui_update
	def_method add, gui/gui_add
	def_method gui, gui/gui

	def_class region
	def_method clip, gui/region_clip_rect
	def_method remove, gui/region_remove_rect
	def_method cut, gui/region_cut_rect
	def_method copy, gui/region_copy_rect
	def_method paste, gui/region_paste_rect
	def_method free, gui/region_free
	def_method copy_region, gui/region_copy_region
	def_method paste_region, gui/region_paste_region
	def_method remove_region, gui/region_remove_region

	def_class ctx
	def_method box, gui/ctx_box
	def_method filled_box, gui/ctx_filled_box
	def_method set_color, gui/ctx_set_color

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

	def_structure	gui_statics
		def_long	gui_statics_sdl_funcs
		def_struct	gui_statics_rect_heap, hp_heap
		def_long	gui_statics_window
		def_long	gui_statics_screen
		def_long	gui_statics_renderer
		def_long	gui_statics_old_region
		def_long	gui_statics_x_pos
		def_long	gui_statics_y_pos
		def_long	gui_statics_buttons
		def_long	gui_statics_last_view
	def_structure_end

	def_structure	gui_rect
		def_long	gui_rect_next
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

;;;;;;;;;;;;;;;
; gui mail data
;;;;;;;;;;;;;;;

	def_structure	ev_type
		def_byte	ev_type_mouse
		def_byte	ev_type_key
	def_structure_end

	def_structure	ev_data
		def_long	ev_data_type
		def_long	ev_data_view
		def_long	ev_data_x
		def_long	ev_data_y
		def_long	ev_data_rx
		def_long	ev_data_ry
		def_long	ev_data_buttons
		def_long	ev_data_key
	def_structure_end

;;;;;;;;;;;;
; gui macros
;;;;;;;;;;;;

	%macro nextpatch 2
		;inputs
		;%1 = node
		;%2 = last
		;trashes
		;%1, %2

		vp_cpy %1, %2
		vp_cpy [%1 + gui_rect_next], %1
		breakif %1, ==, 0
	%endmacro

	%macro removepatch 2
		;inputs
		;%1 = node
		;%2 = last
		;trashes
		;%1

		vp_cpy [%1 + gui_rect_next], %1
		vp_cpy %1, [%2 + gui_rect_next]
		vp_cpy %2, %1
	%endmacro

	%macro addpatch 3
		;inputs
		;%1 = list
		;%2 = node
		;%3 = temp
		;trashes
		;%3

		vp_cpy [%1 + gui_rect_next], %3
		vp_cpy %3, [%2 + gui_rect_next]
		vp_cpy %2, [%1 + gui_rect_next]
	%endmacro

%endif
