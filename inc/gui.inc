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
		GUI_STATICS_RENDERER:			resq	1
		gui_statics_size:
	endstruc

	struc GUI_PATCH
		gui_patch_next:		resq	1
		GUI_PATCH_X:		resq	1
		GUI_PATCH_Y:		resq	1
		GUI_PATCH_X1:		resq	1
		GUI_PATCH_Y1:		resq	1
		GUI_PATCH_SIZE:
	endstruc

	struc GUI_VIEW
		GUI_VIEW_NODE:				resb	ln_node_size
		GUI_VIEW_PARENT:			resq	1
		GUI_VIEW_LIST:				resb	lh_list_size
		GUI_VIEW_DIRTY_LIST:		resq	1
		GUI_VIEW_TRANSPARENT_LIST:	resq	1
		gui_view_x:					resq	1
		gui_view_y:					resq	1
		gui_view_w:					resq	1
		gui_view_h:					resq	1
		GUI_VIEW_CTX_X:				resq	1
		GUI_VIEW_CTX_Y:				resq	1
		gui_view_draw:				resq	1
		GUI_VIEW_SIZE:
	endstruc

	struc GUI_CTX
		gui_ctx_sdl_ctx:		resq	1
		gui_ctx_dirty_region:	resq	1
		GUI_CTX_X:				resq	1
		GUI_CTX_Y:				resq	1
		GUI_CTX_SIZE:
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
