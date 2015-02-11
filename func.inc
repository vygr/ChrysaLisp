%ifndef FUNC_1234
    %define FUNC_1234

%include "task.inc"

;;;;;;;;;;;;;;
; kernel calls
;;;;;;;;;;;;;;

	KERNEL_MAIL_ALLOC		equ 0
	KERNEL_MAIL_FREE		equ 1
	KERNEL_MAIL_SEND		equ 2
	KERNEL_MAIL_READ		equ 3
	KERNEL_TASK_START		equ 4
	KERNEL_TASK_STOP		equ 5
	KERNEL_TASK_SUSPEND		equ 6
	KERNEL_TASK_RESUME		equ 7
	KERNEL_TASK_DESHEDULE	equ 8
	KERNEL_FUNCTION_LOAD	equ 9

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
		FN_HEADER_NEXT:			resq 1
		FN_HEADER_LENGTH:		resq 1
		FN_HEADER_ENTRY:		resq 1
		FN_HEADER_LINKS:		resq 1
		FN_HEADER_PATHNAME:
	endstruc

;;;;;;;;;;;;;
; func macros
;;;;;;;;;;;;;

	%macro fn_call 1
		;inputs
		;%1 = function path name

		%assign %%__f -1
		%assign %%__i 0
		%rep __ftotal
			%deftok %%__path __func_str%[%%__i]
			%ifidn %%__path, %1
				%assign %%__f %%__i
				%exitrep
			%endif
			%assign %%__i %%__i+1
		%endrep
		%if %%__f = -1
			%assign %%__f __ftotal
			%assign __ftotal __ftotal+1
			%defstr __func_str%[%%__f] %1
		%endif
		vp_call [rel __func%[%%__f]]
	%endmacro

	%macro fn_function 1
		;inputs
		;%1 = function path name
			BITS 64
			%assign __ftotal 0
			align 8, db 0
		__func_start:
			dq	-1
			dq	__func_end - __func_start
			dq	__func_entry - __func_start
			dq	__func_links - __func_start
			db	%1
			db	0
			align 8, db 0
		__func_entry:
	%endmacro

	%macro fn_function_end 0
			align 8, db 0
		__func_links:
			%assign %%__i 0
			%rep __ftotal
			__func%[%%__i]:
				dq	-1
				db	__func_str%[%%__i]
				db	0
				align 8, db 0
				%assign %%__i %%__i+1
			%endrep
			dq	0
		__func_end:
	%endmacro

%endif
