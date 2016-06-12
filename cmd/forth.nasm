%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_slave.inc'

	fn_function cmd/forth

		ptr slave
		ptr stream
		ptr vector
		ptr string
		ulong length

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave != 0}
			;set up input stream stack
			static_call string, create_from_file, {"cmd/forth.f"}, {string}
			static_call stream, create, {string, 0, &string->string_data, string->string_length}, {stream}
			static_call vector, create, {}, {vector}
			static_call vector, push_back, {vector, stream}

			;app event loop
			loop_start
				;priority to stack input
				;this allows forth to push include files on this input stack
				loop_start
					static_call vector, get_length, {vector}, {length}
					breakif {!length}
					static_call vector, get_back, {vector}, {stream}
					static_call vector, pop_back, {vector}
					local_call input, {slave, stream}, {r0, r1}
				loop_end

				;read stdin, exit if EOF
				static_call slave, stdin, {slave}, {stream}
				breakif {!stream}
				local_call input, {slave, stream}, {r0, r1}
			loop_end

			;clean up
			static_call vector, deref, {vector}
			static_call slave, deref, {slave}
		endif
		pop_scope
		return

	input:
		;inputs
		;r0 = slave
		;r1 = stream

		buffer_size equ 120

		const char_lf, 10

		ptr slave
		ptr stream
		pubyte charp
		ulong length
		struct buffer, buffer

		push_scope
		retire {r0, r1}, {slave, stream}

		loop_start
			static_call stream, read_line, {stream, &buffer, buffer_size - 1}, {length}
			breakif {length == -1}
			assign {&buffer + length}, {charp}
			assign {char_lf}, {*charp}
			static_call slave, stdout, {slave, &buffer, length + 1}
		loop_end
		static_call stream, deref, {stream}

		pop_scope
		return

%if 0
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
		offset header_name
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
%endif

	fn_function_end
