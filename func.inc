%ifndef FUNC_1234
    %define FUNC_1234

%include "task.inc"

;;;;;;;;;;;;;;
; kernel calls
;;;;;;;;;;;;;;

	KERNEL_ALLOC_MAIL	equ 1
	KERNEL_FREE_MAIL	equ 2
	KERNEL_SEND_MAIL	equ 3
	KERNEL_RECEIVE_MAIL	equ 4

	%macro kn_call 1
		;inputs
		;r15 = task control node
		
		vp_cpy [r15 + TK_NODE_KERNEL], r14
		vp_call [r14 + (%1 *8)]
	%endmacro
		
;;;;;;;;;;;;;;;;;
; func structures
;;;;;;;;;;;;;;;;;

	struc FN_HEADER
		FN_HEADER_LENGHTH:	resq 1
		FN_HEADER_NAME:
	endstruc

;;;;;;;;;;;;;
; func macros
;;;;;;;;;;;;;

	%macro fn_function 1
		;inputs
		;%1 = function path name
			BITS 64
			%push func
			align 8
		%$func_start:
			dq	%$func_end-%$func_start
			db	%1, 0
			align 8
	%endmacro

	%macro fn_function_end 0
			align 8
		%$func_end:
			%pop func
	%endmacro

%endif
