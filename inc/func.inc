%ifndef FUNC_1234
	%define FUNC_1234

%include 'inc/vp.inc'
%include 'inc/code.inc'
%include 'inc/syscall.inc'

;;;;;;;;;;;;;;;;;;;;;;
; kernel mail funcions
;;;;;;;;;;;;;;;;;;;;;;

	struc KN_CALL
		KN_CALL_TASK_OPEN:					resb 1
		KN_CALL_TASK_CHILD:					resb 1
		KN_CALL_LINK_ROUTE:					resb 1
		KN_CALL_GUI_UPDATE:					resb 1
	endstruc

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

	struc kn_data
		kn_data_kernel_user:				resq 1
		kn_data_kernel_reply:				resq 2
		kn_data_kernel_function:			resq 1
	endstruc

	struc kn_data_task_open, kn_data_size
		kn_data_task_open_pathname:
	endstruc

	struc kn_data_task_open_reply
		kn_data_task_open_reply_user:		resq 1
		kn_data_task_open_reply_mailboxid:	resq 2
	endstruc

	struc kn_data_task_child, kn_data_size
		kn_data_task_child_pathname:
	endstruc

	struc kn_data_task_child_reply
		kn_data_task_child_reply_user:		resq 1
		kn_data_task_child_reply_mailboxid:	resq 2
	endstruc

	struc kn_data_link_route
		kn_data_link_route_header:			resb kn_data_size
		kn_data_link_route_origin:			resq 1
		kn_data_link_route_via:				resq 1
		kn_data_link_route_hops:			resq 1
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

		%assign %%_i 0
		%rep _ftotal
			%deftok %%_path _func_str%[%%_i]
			%ifidn %%_path, %1
				%exitrep
			%else
				%assign %%_i %%_i+1
			%endif
		%endrep
		%if %%_i = _ftotal
			%assign _ftotal _ftotal+1
			%defstr _func_str%[%%_i] %1
		%endif
		vp_cpy [rel __func%[%%_i]], %2
	%endmacro

	%macro fn_call 1
		;inputs
		;%1 = function path name

		%assign %%_i 0
		%rep _ftotal
			%deftok %%_path _func_str%[%%_i]
			%ifidn %%_path, %1
				%exitrep
			%else
				%assign %%_i %%_i+1
			%endif
		%endrep
		%if %%_i = _ftotal
			%assign _ftotal _ftotal+1
			%defstr _func_str%[%%_i] %1
		%endif
		vp_call [rel __func%[%%_i]]
	%endmacro

	%macro fn_jmp 1
		;inputs
		;%1 = function path name

		%assign %%_i 0
		%rep _ftotal
			%deftok %%_path _func_str%[%%_i]
			%ifidn %%_path, %1
				%exitrep
			%else
				%assign %%_i %%_i+1
			%endif
		%endrep
		%if %%_i = _ftotal
			%assign _ftotal _ftotal+1
			%defstr _func_str%[%%_i] %1
		%endif
		vp_jmp [rel __func%[%%_i]]
	%endmacro

	%macro fn_function 1
		;inputs
		;%1 = function path name
		%defstr _func_name %1
			BITS 64
			%assign _ftotal 0
			align 8, db 0
		_func_start:
			dq	-1
			dq	_func_end - _func_start
			dq	_func_entry - _func_start
			dq	_func_links - _func_start
			db	_func_name
			db	0
			align 8, db 0
		_func_entry:
	%endmacro

	%macro fn_function_end 0
			align 8, db 0
		_func_links:
			%assign %%_i 0
			%rep _ftotal
			__func%[%%_i]:
				dq	-1
				db	_func_str%[%%_i]
				db	0
				align 8, db 0
				%assign %%_i %%_i+1
			%endrep
			dq	0
		_func_end:
	%endmacro

%endif
