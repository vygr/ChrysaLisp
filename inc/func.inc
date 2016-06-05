%ifndef FUNC_1234
%define FUNC_1234

%include 'inc/vp.inc'
%include 'inc/code.inc'
%include 'inc/syscall.inc'
%include 'inc/task.inc'

%xdefine debug_mode

;;;;;;;;;;;;;;;;;;;;;;
; kernel mail funcions
;;;;;;;;;;;;;;;;;;;;;;

	def_enum
		enum kn_call_callback
		enum kn_call_task_open
		enum kn_call_task_child
		enum kn_call_task_route
	def_enum_end

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

	def_structure kn_msg, msg_header
		ulong kn_msg_user
		struct kn_msg_reply_id, id
		struct kn_msg_reply_size, null
		ulong kn_msg_function
	def_structure_end

	def_structure kn_msg_callback, kn_msg
		ptr kn_msg_callback_addr
	def_structure_end

	def_structure kn_msg_open, kn_msg
		struct kn_msg_open_pathname, null
	def_structure_end

	def_structure kn_msg_child, kn_msg
		struct kn_msg_child_pathname, null
	def_structure_end

	def_structure kn_msg_link_route, kn_msg
		ulong kn_msg_link_route_origin
		ulong kn_msg_link_route_via
		ulong kn_msg_link_route_hops
	def_structure_end

;;;;;;;;;;;;;;;;;
; func structures
;;;;;;;;;;;;;;;;;

	def_structure fn_header, ln_fnode
		int fn_header_length
		int fn_header_entry
		int fn_header_links
		int fn_header_paths
		struct fn_header_pathname, null
	def_structure_end

;;;;;;;;;;;;;
; func macros
;;;;;;;;;;;;;

	%macro fn_add_string 1
		;inputs
		;%1 = string

		%assign _string_num_ 0
		%rep _string_total_
			%ifidn _string_str_%[_string_num_]_, %1
				%exitrep
			%else
				%assign _string_num_ _string_num_ + 1
			%endif
		%endrep
		%if _string_num_ = _string_total_
			%xdefine _string_str_%[_string_num_]_ %1
			%assign _string_total_ _string_total_ + 1
		%endif
	%endmacro

	%macro fn_string 2
		;inputs
		;%1 = string
		;%2 = register

		fn_add_string %1
		vp_rel _ref_%[_string_num_]_string, %2
	%endmacro

	%macro fn_add_path 1
		;inputs
		;%1 = function path name

		%assign _path_num_ 0
		%rep _path_total_
			%ifidn _path_str_%[_path_num_]_, %1
				%exitrep
			%else
				%assign _path_num_ _path_num_ + 1
			%endif
		%endrep
		%if _path_num_ = _path_total_
			%xdefine _path_str_%[_path_num_]_ %1
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
			%ifidn _path_str_%[_link_%[_link_num_]_path_num_]_, %1
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
		vp_cpy_rel _ref_%[_link_num_]_link, %2
	%endmacro

	%macro fn_call 1
		;inputs
		;%1 = function path name

		fn_find_link %1
		vp_call_rel _ref_%[_link_num_]_link
	%endmacro

	%macro fn_jmp 1
		;inputs
		;%1 = function path name

		fn_find_link %1
		vp_jmp_rel _ref_%[_link_num_]_link
	%endmacro

	%macro fn_function 1
		;inputs
		;%1 = function path name

		BITS 64
		%defstr _func_name %1
		%assign _link_total_ 0
		%assign _path_total_ 0
		%assign _string_total_ 0
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
		;define the operators
		;values are precidence, assoiciativity
		push_scope
		operator ., 1, 0, compile_field
		operator ->, 1, 0, compile_member
		operator [], 1, 0, compile_index
		operator :, 2, 1, compile_uaddrof
		operator _, 2, 1, compile_uminus
		operator #, 2, 1, compile_uderef
		operator ~, 2, 1, compile_unot
		operator !, 2, 1, compile_ulnot
		operator *, 3, 0, compile_mul
		operator /, 3, 0, compile_div
		operator %, 3, 0, compile_rem
		operator +, 4, 0, compile_plus
		operator -, 4, 0, compile_minus
		operator <<, 5, 0, compile_lshift
		operator >>, 5, 0, compile_rshift
		operator >>>, 5, 0, compile_arshift
		operator <, 6, 0, compile_lt
		operator >, 6, 0, compile_gt
		operator <=, 6, 0, compile_le
		operator >=, 6, 0, compile_ge
		operator ==, 7, 0, compile_eq
		operator !=, 7, 0, compile_ne
		operator &, 8, 0, compile_and
		operator ^, 9, 0, compile_xor
		operator |, 10, 0, compile_or
		operator &&, 11, 0, compile_land
		operator ||, 12, 0, compile_lor
		operator ?, 13, 0, compile_ternary
		operator (, 14
		operator ), 14
		operator [, 14
		operator ], 14
		push_scope
	%endmacro

	%macro fn_function_end 0
		pop_scope
		pop_scope
		%assign %%i 0
		%rep _string_total_
	_ref_%[%%i]_string:
			db	_string_str_%[%%i]_
			db	0
			%assign %%i %%i + 1
		%endrep
		align 8, db 0
	_func_links:
		%assign %%i 0
		%rep _link_total_
	_ref_%[%%i]_link:
			dq	_ref_%[_link_%[%%i]_path_num_]_path - $
			%assign %%i %%i + 1
		%endrep
		dq	0
	_func_paths:
		%assign %%i 0
		%rep _path_total_
	_ref_%[%%i]_path:
			%defstr %%p _path_str_%[%%i]_
			db	%%p
			db	0
			%assign %%i %%i + 1
		%endrep
	_func_end:
	%endmacro

	%macro fn_debug_long 1-2 0
		;%1 = debug string
		;%2 = debug int

		%ifdef debug_mode
			vp_push r0, r1, r2, r3, r5, %2
			vp_rel _func_start + fn_header_pathname, r0
			vp_cpy __LINE__, r1
			fn_string %1, r2
			vp_pop r3
			s_call sys_io, debug_long, {r0, r1, r2, r3}
			vp_pop r0, r1, r2, r3, r5
		%endif
	%endmacro

	%macro fn_debug_str 1-2 ''
		;%1 = debug string
		;%2 = debug string

		%ifdef debug_mode
			%ifidn %2, ''
				vp_push r0, r1, r2, r3, r5
				vp_rel _func_start + fn_header_pathname, r0
				vp_cpy __LINE__, r1
				fn_string %1, r2
				fn_string '', r3
				s_call sys_io, debug_str, {r0, r1, r2, r3}
				vp_pop r0, r1, r2, r3, r5
			%else
				vp_push r0, r1, r2, r3, r5, %2
				vp_rel _func_start + fn_header_pathname, r0
				vp_cpy __LINE__, r1
				fn_string %1, r2
				vp_pop r3
				s_call sys_io, debug_str, {r0, r1, r2, r3}
				vp_pop r0, r1, r2, r3, r5
			%endif
		%endif
	%endmacro

	%macro abort 0-1 'Abort !'
		;%1 = debug string

		fn_debug_str %1
		sys_exit 1
	%endmacro

	%macro assert 3-4 'Assert Failure !'
		;%1 = val1
		;%2 = condition
		;%3 = val2
		;%4 = debug string

		%ifdef debug_mode
			ifnot %1, %2, %3
				abort %4
			endif
		%endif
	%endmacro

%endif
