%ifndef FUNC_1234
	%define FUNC_1234

%include 'inc/vp.inc'
%include 'inc/code.inc'
%include 'inc/syscall.inc'
%include 'inc/task.inc'

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

	%macro fn_add_link 1
		;inputs
		;%1 = function path name

		%assign _link_num 0
		%rep _link_total
			%deftok %%_path _func_str%[_link_num]
			%ifidn %%_path, %1
				%exitrep
			%else
				%assign _link_num _link_num + 1
			%endif
		%endrep
		%if _link_num = _link_total
			%assign _link_total _link_total + 1
			%defstr _func_str%[_link_num] %1
		%endif
	%endmacro

	%macro fn_bind 2
		;inputs
		;%1 = function path name
		;%2 = register

		fn_add_link %1
		vp_cpy [rel __func%[_link_num]], %2
	%endmacro

	%macro fn_call 1
		;inputs
		;%1 = function path name

		fn_add_link %1
		vp_call [rel __func%[_link_num]]
	%endmacro

	%macro fn_jmp 1
		;inputs
		;%1 = function path name

		fn_add_link %1
		vp_jmp [rel __func%[_link_num]]
	%endmacro

	%macro fn_function 1-2 debug_enter
		;inputs
		;%1 = function path name
		;%2 = debug option
		%defstr _func_name %1
			BITS 64
			%assign _link_total 0
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
			%rep _link_total
			__func%[%%_i]:
				dq	__func%[%%_i]_name - $
				%assign %%_i %%_i+1
			%endrep
			dq	0
			%assign %%_i 0
			%rep _link_total
			__func%[%%_i]_name:
				db	_func_str%[%%_i]
				db	0
				%assign %%_i %%_i+1
			%endrep
		_func_end:
	%endmacro

	%macro fn_debug 1-2 0
		;%1 debug string
		;%2 debug value
		%defstr _debug_string_ %1
			vp_jmp %%over_end
		%%over:
			%defstr %%_l __LINE__
			db ': ', %%_l, ' : ', _debug_string_, ' -> ', 0
		%%over_end:
			vp_push r0, r1, r2, r3, r5
			vp_push %2

			;tab in by stack depth
			fn_bind sys/task_statics, r2
			vp_cpy [r2 + tk_statics_current_tcb], r2
			vp_add tk_node_size - (7*8), r2
			vp_sub r4, r2
			if r2, >, tk_stack_size
				;must be kernel or stack extented
				vp_xor r2, r2
			endif
			vp_cpy ' ', r0
			vp_cpy 1, r1
			loop_while r2, >, 0
				fn_call sys/write_char
				vp_sub 8, r2
			loop_end

			vp_lea [rel _func_start + fn_header_pathname], r0
			vp_cpy 1, r1
			fn_call sys/write_string
			vp_lea [rel %%over], r0
			vp_cpy 1, r1
			fn_call sys/write_string
			vp_pop r0
			vp_cpy 1, r1
			vp_cpy 16, r2
			fn_call sys/write_number
			vp_cpy 10, r0
			fn_call sys/write_char
			vp_pop r0, r1, r2, r3, r5
	%endmacro

%endif
