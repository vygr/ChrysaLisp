%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'inc/list.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'cmd/cmd.inc'

	fn_function cmd/forth

		buffer_size equ 120

		def_structure shared
			struct shared_stdout_id, mailbox_id
			struct shared_stderr_id, mailbox_id
			ulong shared_stdout_seqnum
			struct shared_buffer, buffer
		def_structure_end

		struct shared_data, shared
		struct stdin_list, lh_list
		ulong stdin_seqnum
		ptr msg
		ptr stream
		ptr vector
		ptr string
		ulong length
		pubyte charp

		;init app vars
		push_scope
		assign {0}, {shared_data.shared_stdout_seqnum}
		assign {0}, {stdin_seqnum}
		static_call sys_list, init, {&stdin_list}

		;read stdout, stderr and command line msg
		static_call sys_mail, mymail, {}, {msg}
		assign {msg->cmd_mail_stdout_id.mb_mbox}, {shared_data.shared_stdout_id.mb_mbox}
		assign {msg->cmd_mail_stdout_id.mb_cpu}, {shared_data.shared_stdout_id.mb_cpu}
		assign {msg->cmd_mail_stderr_id.mb_mbox}, {shared_data.shared_stderr_id.mb_mbox}
		assign {msg->cmd_mail_stderr_id.mb_cpu}, {shared_data.shared_stderr_id.mb_cpu}

		;send back ack
		assign {msg->cmd_mail_reply_id.mb_mbox}, {msg->ml_msg_dest.mb_mbox}
		assign {msg->cmd_mail_reply_id.mb_cpu}, {msg->ml_msg_dest.mb_cpu}
		static_call sys_mail, send, {msg}

		;set up input stream stack
		static_call string, create_from_file, {"cmd/forth.f"}, {string}
		static_call stream, create_from_string, {string}, {stream}
		static_call vector, create, {}, {vector}
		static_call vector, push_back, {vector, stream}

		;app event loop
		loop_start
			;priority to stack input
			loop_start
				static_call vector, get_length, {vector}, {length}
				breakif {length == 0}
				static_call vector, get_back, {vector}, {stream}
				static_call stream, read_line, {stream, shared_data.shared_buffer, buffer_size - 1}, {length}
				if {length == 0}
					static_call vector, pop_back, {vector}
				else
					assign {&shared_data.shared_buffer + length}, {charp}
					assign {0}, {*charp}
					local_call stdin, {&shared_data}, {r0}
				endif
				static_call stream, deref, {stream}
				static_call sys_task, yield, {}
			loop_end

			;read stdin, see if next in sequence
			static_call sys_mail, mymail, {}, {msg}
			static_call cmd, next_msg, {&stdin_list, msg, stdin_seqnum}, {msg}
			if {msg != 0}
				;got next stdin message
				local_call stdin, {&msg->cmd_mail_string}, {r0}
				assign {stdin_seqnum + 1}, {stdin_seqnum}
			endif

			;free input stream mail
			static_call sys_mem, free, {msg}
		loop_end

		pop_scope
		vp_ret

	stdin:
		;inputs
		;r0 = shared data

		ptr shared

		push_scope
		retire {r0}, {shared}
		pop_scope
		vp_ret

	stdout:
		;inputs
		;r0 = shared data
		;r1 = cstr data

		ptr shared
		ptr data
		ulong length
		ptr msg

		push_scope
		retire {r0, r1}, {shared, data}
		static_call sys_string, length, {data}, {length}
		static_call sys_mail, alloc, {}, {msg}
		assign {cmd_mail_size + length + 1}, {msg->ml_msg_length}
		assign {shared->shared_stdout_id.mb_mbox}, {msg->ml_msg_dest.mb_mbox}
		assign {shared->shared_stdout_id.mb_cpu}, {msg->ml_msg_dest.mb_cpu}
		static_call sys_mem, copy, {data, &msg->ml_msg_data, length + 1}, {_, _}
		assign {shared->shared_stdout_seqnum}, {msg->cmd_mail_seqnum}
		assign {shared->shared_stdout_seqnum + 1}, {shared->shared_stdout_seqnum}
		static_call sys_mail, send, {msg}
		pop_scope
		vp_ret

	stderr:
		;inputs
		;r0 = shared data
		;r1 = cstr data

		ptr shared
		ptr data
		ulong length
		ptr msg

		push_scope
		retire {r0, r1}, {shared, data}
		static_call sys_string, length, {data}, {length}
		static_call sys_mail, alloc, {}, {msg}
		assign {cmd_mail_size + length + 1}, {msg->ml_msg_length}
		assign {shared->shared_stderr_id.mb_mbox}, {msg->ml_msg_dest.mb_mbox}
		assign {shared->shared_stderr_id.mb_cpu}, {msg->ml_msg_dest.mb_cpu}
		static_call sys_mem, copy, {data, &msg->ml_msg_data, length + 1}, {_, _}
		static_call sys_mail, send, {msg}
		pop_scope
		vp_ret

;;;;;;;;;;
; forth vm
;;;;;;;;;;

	%xdefine t1 r0
	%xdefine t2 r1
	%xdefine t3 r2
	%xdefine sp r3
	%xdefine rp r4

	%macro loadsp 1-2 0
		vp_cpy [sp + (%2 * long_size)], %1
	%endmacro

	%macro storesp 1-2 0
		vp_cpy %1, [sp + (%2 * long_size)]
	%endmacro

	%macro decsp 0-1 1
		vp_sub (%1 * long_size), sp
	%endmacro

	%macro incsp 0-1 1
		vp_add (%1 * long_size), sp
	%endmacro

	%macro pushsp 1
		decsp
		storesp %1
	%endmacro

	%macro popsp 1
		loadsp %1
		incsp
	%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dictionary building macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;format of dictionary entry flag byte
	F_IMMED equ 0x80
	F_HIDDEN equ 0x20
	F_LENMASK equ 0x1f

	;format of header
	def_structure header
		long header_link
		ubyte header_nsize
		struct header_name, null
	def_structure_end

	;format of header backwards from code entry point
	def_structure xt
		long xt_compile
		long xt_length
		long xt_body
	def_structure_end

	%macro defword 4
		%push newword
		%strlen %%l %1
		align long_size
	dic_%3:
		dq %$code_end - dic_%3	; LATEST list link
		db %%l + %2			; flags + length byte
		db %1				; the name
		dq %3 - $			; body pointer
		dq %$code_end - %3	; code length
		dq %4 - $			; compile action word
	%3:
	%endm					; assembler code follows

	%macro defword_end 0
	%$code_end:
		%pop
	%endm

	%macro defvar 4
		defword %1, %2, %3, word_inline_comma
			vp_rel var_%3, t1
			pushsp t1
			vp_ret
		defword_end
		align long_size
	var_%3:
		dq %4
	%endm

	%macro defconst 4
		defword %1, %2, %3, word_inline_comma
			vp_cpy %4, t1
			pushsp t1
			vp_ret
		defword_end
	%endm

;;;;;;;;;;;;;;
; memory words
;;;;;;;;;;;;;;

	defword "@", 0, word_fetch, word_inline_comma
		;( a -- [a] )
		loadsp t1
		vp_cpy [t1], t1
		storesp t1
		vp_ret
	defword_end

	defword "c@", 0, word_fetchbyte, word_inline_comma
		;( a -- [a].b )
		loadsp t1
		vp_cpy_ub [t1], t1
		storesp t1
		vp_ret
	defword_end

	defword "!", 0, word_store, word_inline_comma
		;( a b -- )
		popsp t1
		popsp t2
		vp_cpy t1, [t2]
		vp_ret
	defword_end

	defword "c!", 0, word_storebyte, word_inline_comma
		;( a b -- )
		popsp t1
		popsp t2
		vp_cpy_b t1, [t2]
		vp_ret
	defword_end

;;;;;;;;;;;
; alu words
;;;;;;;;;;;

	defword "+", 0, word_add, word_inline_comma
		;( a b -- (a + b))
		popsp t1
		loadsp t2
		vp_add t1, t2
		storesp t2
		vp_ret
	defword_end

	defword "-", 0, word_sub, word_inline_comma
		;( a b -- (a - b))
		popsp t1
		loadsp t2
		vp_sub t1, t2
		storesp t2
		vp_ret
	defword_end

	defword "*", 0, word_mull, word_inline_comma
		;( a b -- (a * b))
		popsp t1
		loadsp t2
		vp_mul t1, t2
		storesp t2
		vp_ret
	defword_end

	defword "/", 0, word_div, word_inline_comma
		;( a b -- (a / b))
		popsp t2
		loadsp t1
		vp_xor t3, t3
		vp_div t2, t3, t1
		storesp t1
		vp_ret
	defword_end

	defword "mod", 0, word_mod, word_inline_comma
		;( a b -- (a % b))
		popsp t2
		loadsp t1
		vp_xor t3, t3
		vp_div t2, t3, t1
		storesp t3
		vp_ret
	defword_end

;;;;;;;;;;;;;;;;;;;
; stack order words
;;;;;;;;;;;;;;;;;;;

	defword "dup", 0, word_dup, word_inline_comma
		;( a -- a a )
		loadsp t1
		pushsp t1
		vp_ret
	defword_end

	defword "swap", 0, word_swap, word_inline_comma
		;( a b -- b a )
		loadsp t1
		loadsp t2, 1
		storesp t1, 1
		storesp t2
		vp_ret
	defword_end

	defword "drop", 0, word_drop, word_inline_comma
		;( a -- )
		incsp
		vp_ret
	defword_end

	defword "over", 0, word_over, word_inline_comma
		;( a b -- a b a )
		loadsp t1, 1
		pushsp t1
		vp_ret
	defword_end

	defword "rot", 0, word_rot, word_inline_comma
		;( a b c -- b c a )
		loadsp t1, 2
		loadsp t2, 1
		loadsp t3
		storesp t2, 2
		storesp t3, 1
		storesp t1
		vp_ret
	defword_end

	defword "-rot", 0, word_nrot, word_inline_comma
		;( a b c -- c a b )
		loadsp t1, 2
		loadsp t2, 1
		loadsp t3
		storesp t3, 2
		storesp t1, 1
		storesp t2
		vp_ret
	defword_end

	defword "?dup", 0, word_qdup, word_inline_comma
		;( a -- a a | 0 )
		loadsp t1
		if t1, !=, 0
			pushsp t1
		endif
		vp_ret
	defword_end

	defword "!?dup", 0, word_nqdup, word_inline_comma
		;( a -- a a | 0 )
		loadsp t1
		if t1, ==, 0
			pushsp t1
		endif
		vp_ret
	defword_end

	defword "nip", 0, word_nip, word_inline_comma
		;( a b -- b )
		loadsp t1
		incsp
		storesp t1
		vp_ret
	defword_end

	defword "tuck", 0, word_tuck, word_inline_comma
		;( a b -- b a b )
		loadsp t1, 1
		loadsp t2
		decsp
		storesp t2, 2
		storesp t1, 1
		storesp t2
		vp_ret
	defword_end

	defword "pick", 0, word_pick, word_inline_comma
		;( a0 .. an n -- a0 .. an a0 )
		popsp t1
		loadsp t2, t1
		pushsp t2
		vp_ret
	defword_end

;;;;;;;;;;;;;;;
; compile words
;;;;;;;;;;;;;;;

	defword "call,", 0, word_call_comma, word_call_comma
		vp_ret
	defword_end

	defword "inline,", 0, word_inline_comma, word_call_comma
		vp_ret
	defword_end

	fn_function_end
