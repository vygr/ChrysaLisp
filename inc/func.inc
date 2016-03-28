%ifndef FUNC_1234
%define FUNC_1234

%include 'inc/vp.inc'
%include 'inc/code.inc'
%include 'inc/syscall.inc'
%include 'inc/task.inc'

;%define debug_function_enter
%define debug_mode

;;;;;;;;;;;;;;;;;;;;;;
; kernel mail funcions
;;;;;;;;;;;;;;;;;;;;;;

	def_structure	kn_call
		def_byte	kn_call_callback
		def_byte	kn_call_task_open
		def_byte	kn_call_task_child
		def_byte	kn_call_task_route
	def_structure_end

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

	def_structure	kn_data
		def_long	kn_data_kernel_user
		def_struct	kn_data_kernel_reply, mailbox_id
		def_long	kn_data_kernel_function
	def_structure_end

	def_structure	kn_data_callback, kn_data
		def_long	kn_data_callback_addr
	def_structure_end

	def_structure	kn_data_task_open, kn_data
		def_struct	kn_data_task_open_pathname, null
	def_structure_end

	def_structure	kn_data_task_open_reply
		def_long	kn_data_task_open_reply_user
		def_struct	kn_data_task_open_reply_mailboxid, mailbox_id
	def_structure_end

	def_structure	kn_data_task_child, kn_data
		def_struct	kn_data_task_child_pathname, null
	def_structure_end

	def_structure	kn_data_task_child_reply
		def_long	kn_data_task_child_reply_user
		def_struct	kn_data_task_child_reply_mailboxid, mailbox_id
	def_structure_end

	def_structure	kn_data_link_route
		def_struct	kn_data_link_route_header, kn_data
		def_long	kn_data_link_route_origin
		def_long	kn_data_link_route_via
		def_long	kn_data_link_route_hops
	def_structure_end

;;;;;;;;;;;;;;;;;
; func structures
;;;;;;;;;;;;;;;;;

	def_structure	fn_header, ln_fnode
		def_int		fn_header_length
		def_int		fn_header_entry
		def_int		fn_header_links
		def_int		fn_header_paths
		def_struct	fn_header_pathname, null
	def_structure_end

;;;;;;;;;;;;;
; func macros
;;;;;;;;;;;;;

	%macro fn_add_path 1
		;inputs
		;%1 = function path name

		%assign _path_num_ 0
		%rep _path_total_
			%deftok %%_path_ _path_str_%[_path_num_]_
			%ifidn %%_path_, %1
				%exitrep
			%else
				%assign _path_num_ _path_num_ + 1
			%endif
		%endrep
		%if _path_num_ = _path_total_
			%defstr _path_str_%[_path_num_]_ %1
			%assign _path_total_ _path_total_ + 1
		%endif
	%endmacro

	%macro fn_add_link 1
		;inputs
		;%1 = function path name

		fn_add_path %1
		%assign _link_%[_link_total_]_path_num_ _path_num_
		%assign _link_total_ _link_total_ + 1
	%endmacro

	%macro fn_find_link 1
		;inputs
		;%1 = function path name

		%assign _link_num_ 0
		%rep _link_total_
			%deftok %%_path_ _path_str_%[_link_%[_link_num_]_path_num_]_
			%ifidn %%_path_, %1
				%exitrep
			%else
				%assign _link_num_ _link_num_ + 1
			%endif
		%endrep
		%if _link_num_ = _link_total_
			fn_add_link %1
		%endif
	%endmacro

	%macro fn_bind 2
		;inputs
		;%1 = function path name
		;%2 = register

		fn_find_link %1
		vp_cpy [rel _func_%[_link_num_]], %2
	%endmacro

	%macro fn_call 1
		;inputs
		;%1 = function path name

		fn_find_link %1
		vp_call [rel _func_%[_link_num_]]
	%endmacro

	%macro fn_jmp 1
		;inputs
		;%1 = function path name

		fn_find_link %1
		vp_jmp [rel _func_%[_link_num_]]
	%endmacro

	%macro fn_find 1
		;inputs
		;%1 = function path name
		;outputs
		;r0 = 0, else function entry point
		;trashes
		;r1-r3, r5-r8

		vp_cpy %1, r0
		static_call sys_load, bind
	%endmacro

	%macro fn_function 1-2 debug_enter
		;inputs
		;%1 = function path name
		;%2 = debug option

		BITS 64
		%defstr _func_name %1
		%assign _link_total_ 0
		%assign _path_total_ 0
		align 8, db 0
	_func_start:
		dq	-1
		dd	_func_end - _func_start
		dd	_func_entry - _func_start
		dd	_func_links - _func_start
		dd	_func_paths - _func_start
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
		%rep _link_total_
	_func_%[%%_i]:
			dq	_func_%[_link_%[%%_i]_path_num_]_path - $
			%assign %%_i %%_i+1
		%endrep
		dq	0
	_func_paths:
		%assign %%_i 0
		%rep _path_total_
	_func_%[%%_i]_path:
			db	_path_str_%[%%_i]_
			db	0
			%assign %%_i %%_i+1
		%endrep
	_func_end:
	%endmacro

	%macro fn_debug 1-2 0
		;%1 = debug string
		;%2 = debug value
		%ifdef debug_mode
			%defstr _debug_string_ %1
			vp_jmp %%over_end
		%%over:
			%defstr %%_l __LINE__
			db ': ', %%_l, ' : ', _debug_string_, ' -> ', 0
		%%over_end:
			vp_push r0, r1, r2, r3, r5
			vp_push %2

			;tab in by stack depth
			static_call sys_task, stack_depth
			if r0, >, 0
				vp_lea [r0 - (8*8)], r2
				vp_cpy ' ', r0
				vp_cpy 2, r1
				loop_while r2, >, 0
					static_call sys_io, char
					vp_sub 8, r2
				loop_end

				vp_adr _func_start + fn_header_pathname, r0
				vp_cpy 2, r1
				static_call sys_io, string
				vp_adr %%over, r0
				vp_cpy 2, r1
				static_call sys_io, string
				vp_pop r0
				vp_cpy 2, r1
				vp_cpy 16, r2
				static_call sys_io, number
				vp_cpy 10, r0
				static_call sys_io, char
			else
				vp_pop r0
			endif
			vp_pop r0, r1, r2, r3, r5
		%endif
	%endmacro

	%macro fn_assert 3-4 Assert Failure !
		;%1 = val1
		;%2 = condition
		;%3 = val2
		;%4 = debug string

		%ifdef debug_mode
			if %1, %2, %3
			else
				fn_debug %4
				sys_exit 1
			endif
		%endif
	%endmacro

%endif
