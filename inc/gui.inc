%ifndef GUI_1234
	%define GUI_1234

%include 'inc/list.inc'
%include 'inc/heap.inc'

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

	struc GUI_STATICS
		gui_statics_sdl_funcs:			resq	1
		gui_statics_patch_heap:			resb	hp_heap_size
		gui_statics_view_heap:			resb	hp_heap_size
		gui_statics_view_list:			resb	lh_list_size
		gui_statics_window:				resq	1
		gui_statics_screen:				resq	1
		gui_statics_renderer:			resq	1
		gui_statics_size:
	endstruc

	struc GUI_PATCH
		gui_patch_next:		resq	1
		gui_patch_x:		resq	1
		gui_patch_y:		resq	1
		gui_patch_x1:		resq	1
		gui_patch_y1:		resq	1
		GUI_PATCH_SIZE:
	endstruc

	struc GUI_VIEW
		gui_view_node:				resb	ln_node_size
		gui_view_parent:			resq	1
		gui_view_list:				resb	lh_list_size
		gui_view_dirty_list:		resq	1
		gui_view_transparent_list:	resq	1
		gui_view_x:					resq	1
		gui_view_y:					resq	1
		gui_view_w:					resq	1
		gui_view_h:					resq	1
		gui_view_ctx_x:				resq	1
		gui_view_ctx_y:				resq	1
		gui_view_draw:				resq	1
		GUI_VIEW_SIZE:
	endstruc

	struc GUI_CTX
		gui_ctx_sdl_ctx:		resq	1
		gui_ctx_dirty_region:	resq	1
		GUI_CTX_X:				resq	1
		GUI_CTX_Y:				resq	1
		gui_ctx_size:
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
		vp_cpy [%1 + gui_patch_next], %1
		breakif %1, ==, 0
	%endmacro

	%macro removepatch 2
		;inputs
		;%1 = node
		;%2 = last
		;trashes
		;%1

		vp_cpy [%1 + gui_patch_next], %1
		vp_cpy %1, [%2 + gui_patch_next]
		vp_cpy %2, %1
	%endmacro

	%macro addpatch 3
		;inputs
		;%1 = list
		;%2 = node
		;%3 = temp
		;trashes
		;%3

		vp_cpy [%1 + gui_patch_next], %3
		vp_cpy %3, [%2 + gui_patch_next]
		vp_cpy %2, [%1 + gui_patch_next]
	%endmacro

%endif
