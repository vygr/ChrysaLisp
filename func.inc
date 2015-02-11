%ifndef FUNC_1234
    %define FUNC_1234

%include "task.inc"

;;;;;;;;;;;;;;
; kernel calls
;;;;;;;;;;;;;;

	KERNEL_ALLOC_MAIL		equ 0
	KERNEL_FREE_MAIL		equ 1
	KERNEL_SEND_MAIL		equ 2
	KERNEL_READ_MAIL		equ 3
	KERNEL_START_TASK		equ 4
	KERNEL_STOP_TASK		equ 5
	KERNEL_SUSPEND_TASK		equ 6
	KERNEL_RESUME_TASK		equ 7
	KERNEL_DESHEDULE_TASK	equ 8
	KERNEL_LOAD_FUNCTION	equ 9
	KERNEL_WRITE_CHAR		equ 10
	KERNEL_WRITE_STRING		equ 11
	KERNEL_WRITE_NUMBER		equ 12
	KERNEL_STRING_COMPARE	equ 13
	KERNEL_STRING_LENGTH	equ 14

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

		%assign %$__fnew -1
		%assign %$__i 0
		%rep %$__ftotal
			%deftok %$__path %$func_str%$__i
			%ifidn %$__path, %1
				%assign %$__fnew %$__i
				%exitrep
			%endif
			%assign %$__i %$__i+1
		%endrep
		%if %$__fnew = -1
			%assign %$__fnew %$__ftotal
			%assign %$__ftotal %$__ftotal+1
			%defstr %$func_str%$__fnew %1
		%endif
		vp_call [rel %$func%$__fnew]
	%endmacro

	%macro fn_function 1
		;inputs
		;%1 = function path name
			BITS 64
			%push func
			%assign %$__ftotal 0
			align 8, db 0
		%$func_start:
			dq	-1
			dq	%$func_end-%$func_start
			dq	%$func_entry-%$func_start
			dq	%$func_links-%$func_start
			db	%1
			db	0
			align 8, db 0
		%$func_entry:
	%endmacro

	%macro fn_function_end 0
			align 8, db 0
		%$func_links:
			%assign %$__i 0
			%rep %$__ftotal
			%$func%$__i:
				dq	-1
				db	%$func_str%$__i
				db	0
				align 8, db 0
				%assign %$__i %$__i+1
			%endrep
			dq	0
		%$func_end:
			%pop func
	%endmacro

%endif
