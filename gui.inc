%ifndef GUI_1234
	%define GUI_1234

%include "list.inc"
%include "heap.inc"

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

	struc GUI_STATICS
		GUI_STATICS_SDL_FUNCS:			resq	1
		GUI_STATICS_PATCH_HEAP:			resb	HP_HEAP_SIZE
		GUI_STATICS_VIEW_HEAP:			resb	GUI_VIEW_SIZE
		GUI_STATICS_VIEW_LIST:			resb	LH_LIST_SIZE
		GUI_STATICS_WINDOW:				resq	1
		GUI_STATICS_SCREEN:				resq	1
		GUI_STATICS_RENDERER:			resq	1
		GUI_STATICS_SIZE:
	endstruc

	struc GUI_PATCH
		GUI_PATCH_NODE:		resb	LN_NODE_SIZE
		GUI_PATCH_X:		resq	1
		GUI_PATCH_Y:		resq	1
		GUI_PATCH_W:		resq	1
		GUI_PATCH_H:		resq	1
		GUI_PATCH_SIZE:
	endstruc

	struc GUI_VIEW
		GUI_VIEW_NODE:		resb	LN_NODE_SIZE
		GUI_VIEW_PARENT:	resq	1
		GUI_VIEW_LIST:		resb	LH_LIST_SIZE
		GUI_DIRTY_LIST:		resb	LH_LIST_SIZE
		GUI_VIEW_X:			resq	1
		GUI_VIEW_Y:			resq	1
		GUI_VIEW_W:			resq	1
		GUI_VIEW_H:			resq	1
		GUI_VIEW_DRAW:		resq	1
		GUI_VIEW_SIZE:
	endstruc

	struc GUI_CTX
		GUI_CTX_SDL_CTX:		resq	1
		GUI_CTX_DIRTY_REGION:	resq	1
		GUI_CTX_X:				resq	1
		GUI_CTX_Y:				resq	1
		GUI_CTX_SIZE:
	endstruc

%endif
