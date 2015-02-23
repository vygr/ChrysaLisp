%ifndef FUNC_1234
    %define FUNC_1234

%include 'vp.inc'
%include 'code.inc'
%include 'syscall.inc'

;;;;;;;;;;;;;;;;;;;;;;
; kernel mail funcions
;;;;;;;;;;;;;;;;;;;;;;

	struc KN_CALL
		KN_CALL_TASK_OPEN:					resb 1
		KN_CALL_TASK_CHILD:					resb 1
		KN_CALL_LINK_ROUTE:					resb 1
	endstruc

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

	struc KN_DATA
		KN_DATA_KERNEL_USER:				resq 1
		KN_DATA_KERNEL_REPLY:				resq 2
		KN_DATA_KERNEL_FUNCTION:			resq 1
		KN_DATA_KERNEL_SIZE:
	endstruc

	struc KN_DATA_TASK_OPEN
		KN_DATA_TASK_OPEN_HEADER:			resb KN_DATA_KERNEL_SIZE
		KN_DATA_TASK_OPEN_PATHNAME:
	endstruc

	struc KN_DATA_TASK_OPEN_REPLY
		KN_DATA_TASK_OPEN_REPLY_USER:		resq 1
		KN_DATA_TASK_OPEN_REPLY_MAILBOXID:	resq 2
		KN_DATA_TASK_OPEN_REPLY_SIZE:
	endstruc

	struc KN_DATA_TASK_CHILD
		KN_DATA_TASK_CHILD_HEADER:			resb KN_DATA_KERNEL_SIZE
		KN_DATA_TASK_CHILD_PATHNAME:
	endstruc

	struc KN_DATA_TASK_CHILD_REPLY
		KN_DATA_TASK_CHILD_REPLY_USER:		resq 1
		KN_DATA_TASK_CHILD_REPLY_MAILBOXID:	resq 2
		KN_DATA_TASK_CHILD_REPLY_SIZE:
	endstruc

	struc KN_DATA_LINK_ROUTE
		KN_DATA_LINK_ROUTE_HEADER:			resb KN_DATA_KERNEL_SIZE
		KN_DATA_LINK_ROUTE_ORIGIN:			resq 1
		KN_DATA_LINK_ROUTE_VIA:				resq 1
		KN_DATA_LINK_ROUTE_HOPS:			resq 1
		KN_DATA_LINK_ROUTE_SIZE:
	endstruc

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

	%macro fn_bind 2
		;inputs
		;%1 = function path name
		;%2 = register

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
		vp_cpy [rel __func%[%%__f]], %2
	%endmacro

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

	%macro fn_jmp 1
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
		vp_jmp [rel __func%[%%__f]]
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
