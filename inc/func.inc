%ifndef FUNC_1234
	%define FUNC_1234

%include 'inc/vp.inc'
%include 'inc/code.inc'
%include 'inc/syscall.inc'

	;%define debug_function_enter

;;;;;;;;;;;;;;;;;;;;;;
; kernel mail funcions
;;;;;;;;;;;;;;;;;;;;;;

	struc kn_call
		fn_call_task_open:					resb 1
		fn_call_task_child:					resb 1
		fn_call_task_route:					resb 1
		fn_call_gui_update:					resb 1
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

	struc fn_header
		fn_header_next:			resq 1
		fn_header_length:		resq 1
		fn_header_entry:		resq 1
		fn_header_links:		resq 1
		fn_header_pathname:
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

	%macro fn_function 1-2 debug_enter
		;inputs
		;%1 = function path name
		;%2 = debug option
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
			%ifdef debug_function_enter
				%ifidn %2, debug_enter
					fn_debug enter
				%elifidn %2, no_debug_enter
				%else
					%fatal No such debug setting, use debug_enter or no_debug_enter
				%endif
			%endif
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

	%macro fn_debug 1
		;%1 debug string
		%defstr _debug_string_ %1
			vp_jmp %%over_end
		%%over:
			db ': ', _debug_string_, 10, 0
		%%over_end:
			vp_push r0, r1, r2, r3
			vp_lea [rel _func_start + fn_header_pathname], r0
			vp_cpy 1, r1
			fn_call sys/write_string
			vp_lea [rel %%over], r0
			vp_cpy 1, r1
			fn_call sys/write_string
			vp_pop r0, r1, r2, r3
	%endmacro

%endif
