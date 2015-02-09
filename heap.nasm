%ifndef HEAP_1234
    %define HEAP_1234

%include "vp.nasm"
%include "code.nasm"
%include "syscall.nasm"
%include "list.nasm"

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

	%macro hp_free_cell 3
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

;;;;;;;;;;;
; heap code
;;;;;;;;;;;

	SECTION .text

hp_init:
	;inputs
	;r0 = heap
	;r1 = cell size
	;r2 = block size
	;outputs
	;r0 = heap
	;r1 = cell size
	;r2 = block size

	vp_cpy 0, long[r0 + HP_HEAP_FREELIST]
	vp_cpy 0, long[r0 + HP_HEAP_BLOCKLIST]
	vp_add HP_CELL_SIZE - 1, r1
	vp_and -HP_CELL_SIZE, r1
	vp_cpy r1, [r0 + HP_HEAP_CELLSIZE]
	vp_cpy r2, [r0 + HP_HEAP_BLOCKSIZE]
	vp_ret

hp_deinit:
	;inputs
	;r0 = heap
	;outputs
	;r0 = heap
	;trashes
	;r0-r3

	vp_cpy r0, r2
	vp_cpy [r2 + HP_HEAP_BLOCKLIST], r1
	loopstart
		breakif r1, e, 0
		vp_cpy [r1 + HP_BLOCK_NEXT], r3
		sys_munmap r1, [r2 + HP_HEAP_BLOCKSIZE]
		vp_cpy r3, r1
	loopend
	vp_ret

hp_free_heap:
	;inputs
	;r0 = heap
	;outputs
	;r0 = heap
	;trashes
	;r1-r3, r5

	vp_cpy 0, r1
	vp_cpy [r0 + HP_HEAP_BLOCKLIST], r2
	loopstart
		breakif r2, e, 0
		vp_lea [r2 + HP_BLOCK_SIZE], r3
		vp_cpy r3, r5
		vp_add [r0 + HP_HEAP_BLOCKSIZE], r5
		repeat
			vp_cpy r1, [r3 + HP_CELL_NEXT]
			vp_cpy r3, r1
			vp_add [r0 + HP_HEAP_CELLSIZE], r3
		until r3, ge, r5
		vp_cpy [r2 + HP_BLOCK_NEXT], r2
	loopend
	vp_cpy r1, [r0 + HP_HEAP_FREELIST]
	vp_ret

hp_alloc_cell:
	;inputs
	;r0 = heap
	;outputs
	;r0 = heap
	;r1 = cell
	;trashes
	;r2-r3

	vp_cpy [r0 + HP_HEAP_FREELIST], r1
	if r1, ne, 0
		vp_cpy [r1 + HP_CELL_NEXT], r2
		vp_cpy r2, [r0 + HP_HEAP_FREELIST]
		vp_ret
	endif
	vp_cpy [r0 + HP_HEAP_BLOCKSIZE], r1
	vp_add HP_BLOCK_SIZE, r1
	vp_cpy r0, r2
	sys_mmap 0, r1, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0
	vp_cpy r0, r1
	vp_cpy r2, r0
	vp_cpy [r0 + HP_HEAP_BLOCKLIST], r2
	vp_cpy r2, [r1 + HP_BLOCK_NEXT]
	vp_cpy r1, [r0 + HP_HEAP_BLOCKLIST]
	vp_add HP_BLOCK_SIZE, r1
	vp_cpy r1, r3
	vp_add [r0 + HP_HEAP_BLOCKSIZE], r3
	vp_xor r2, r2
	repeat
		vp_cpy r2, [r3 + HP_CELL_NEXT]
		vp_cpy r1, r2
		vp_add [r0 + HP_HEAP_CELLSIZE], r1
	until r1, ge, r3
	vp_cpy r2, [r0 + HP_HEAP_FREELIST]
	vp_jmp hp_alloc_cell

%endif
