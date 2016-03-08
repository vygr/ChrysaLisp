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
	def_method draw, gui/gui_draw
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
	def_method filled_box, gui/ctx_filled_box
	def_method set_color, gui/ctx_set_color

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

	struc gui_statics
		gui_statics_sdl_funcs:			resq 1
		gui_statics_rect_heap:			resb hp_heap_size
		gui_statics_window:				resq 1
		gui_statics_screen:				resq 1
		gui_statics_renderer:			resq 1
	endstruc

	struc gui_rect
		gui_rect_next:		resq 1
		gui_rect_x:			resq 1
		gui_rect_y:			resq 1
		gui_rect_x1:		resq 1
		gui_rect_y1:		resq 1
	endstruc

	struc gui_ctx
		gui_ctx_sdl_ctx:		resq 1
		gui_ctx_dirty_region:	resq 1
		gui_ctx_x:				resq 1
		gui_ctx_y:				resq 1
	endstruc

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
