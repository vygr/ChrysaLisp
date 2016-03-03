%ifndef GUI_1234
	%define GUI_1234

%include 'inc/list.inc'
%include 'inc/heap.inc'

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

	struc GUI_STATICS
		GUI_STATICS_SDL_FUNCS:			resq	1
		GUI_STATICS_PATCH_HEAP:			resb	hp_heap_size
		GUI_STATICS_VIEW_HEAP:			resb	hp_heap_size
		GUI_STATICS_VIEW_LIST:			resb	LH_LIST_SIZE
		GUI_STATICS_WINDOW:				resq	1
		GUI_STATICS_SCREEN:				resq	1
		GUI_STATICS_RENDERER:			resq	1
		GUI_STATICS_SIZE:
	endstruc

	struc GUI_PATCH
		GUI_PATCH_NEXT:		resq	1
		GUI_PATCH_X:		resq	1
		GUI_PATCH_Y:		resq	1
		GUI_PATCH_X1:		resq	1
		GUI_PATCH_Y1:		resq	1
		GUI_PATCH_SIZE:
	endstruc

	struc GUI_VIEW
		GUI_VIEW_NODE:				resb	LN_NODE_SIZE
		GUI_VIEW_PARENT:			resq	1
		GUI_VIEW_LIST:				resb	LH_LIST_SIZE
		GUI_VIEW_DIRTY_LIST:		resq	1
		GUI_VIEW_TRANSPARENT_LIST:	resq	1
		GUI_VIEW_X:					resq	1
		GUI_VIEW_Y:					resq	1
		GUI_VIEW_W:					resq	1
		GUI_VIEW_H:					resq	1
		GUI_VIEW_CTX_X:				resq	1
		GUI_VIEW_CTX_Y:				resq	1
		GUI_VIEW_DRAW:				resq	1
		GUI_VIEW_SIZE:
	endstruc

	struc GUI_CTX
		GUI_CTX_SDL_CTX:		resq	1
		GUI_CTX_DIRTY_REGION:	resq	1
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
		vp_cpy [%1 + GUI_PATCH_NEXT], %1
		breakif %1, ==, 0
	%endmacro

	%macro removepatch 2
		;inputs
		;%1 = node
		;%2 = last
		;trashes
		;%1

		vp_cpy [%1 + GUI_PATCH_NEXT], %1
		vp_cpy %1, [%2 + GUI_PATCH_NEXT]
		vp_cpy %2, %1
	%endmacro

	%macro addpatch 3
		;inputs
		;%1 = list
		;%2 = node
		;%3 = temp
		;trashes
		;%3

		vp_cpy [%1 + GUI_PATCH_NEXT], %3
		vp_cpy %3, [%2 + GUI_PATCH_NEXT]
		vp_cpy %2, [%1 + GUI_PATCH_NEXT]
	%endmacro

%endif
