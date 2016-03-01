%ifndef HEAP_1234
	%define HEAP_1234

%include "inc/vp.inc"
%include "inc/code.inc"

;;;;;;;;;;;;;;;;;
; heap structures
;;;;;;;;;;;;;;;;;

	HP_PAGE_SIZE	equ 4*1024

	struc HP_HEAP
		HP_HEAP_FREELIST:	resq 1
		HP_HEAP_BLOCKLIST:	resq 1
		HP_HEAP_CELLSIZE:	resq 1
		HP_HEAP_BLOCKSIZE:	resq 1
		HP_HEAP_SIZE:
	endstruc

	struc HP_CELL
		HP_CELL_NEXT:	resq 1
		HP_CELL_SIZE:
	endstruc

	struc HP_BLOCK
		HP_BLOCK_NEXT:	resq 1
		HP_BLOCK_SIZE:
	endstruc

;;;;;;;;;;;;;
; heap macros
;;;;;;;;;;;;;

	%macro hp_heap_object 1
		align 8
	%1:
		times HP_HEAP_SIZE db 0
	%endmacro

	%macro hp_freecell 3
		;inputs
		;%1 = heap
		;%2 = cell
		;%3 = temp
		;outputs
		;%1 = heap
		;%2 = cell
		;%3 = old first cell

		vp_cpy [%1 + HP_HEAP_FREELIST], %3
		vp_cpy %2, [%1 + HP_HEAP_FREELIST]
		vp_cpy %3, [%2 + HP_CELL_NEXT]
	%endmacro

%endif
