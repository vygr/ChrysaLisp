%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/string.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_label.inc'
%include 'class/class_string.inc'

	fn_function forth/forth

		struct myapp, obj
		long msg
		long window
		long window_panel
		long label
		long panel
		pubyte next
		long string
		long length
		long width
		long height
		long owner
		long keychar
		long line_string
		long new_line_string

		;init app vars
		push_scope
		slot_function class, obj
		static_call obj, init, {&myapp, @_function_}, {_}

		;create my window
		static_call window, create, {}, {window}
		static_call window, get_panel, {window}, {window_panel}
		static_call string, create, {"Terminal"}, {string}
		static_call window, set_title, {window, string}
		static_call string, create, {"Status Text"}, {string}
		static_call window, set_status, {window, string}

		;add my app panel
		static_call flow, create, {}, {panel}
		static_call flow, set_flow_flags, {panel, flow_flag_down | flow_flag_fillw}
		static_call flow, add_back, {panel, window_panel}

		;add terminal lines to my app panel
		assign {$line_list}, {next}
		loop_start
			breakifnot {*next}

			static_call label, create, {}, {label}
			static_call string, create, {next}, {string}
			static_call label, set_text, {label, string}
			static_call label, set_color, {label, 0xff000000}
			static_call label, set_text_color, {label, 0xff00ff00}
			static_call label, set_font, {label, "fonts/OpenSans-Regular.ttf", 12}
			static_call label, add_back, {label, panel}

			static_call sys_string, length, {next}, {length}
			assign {next + length + 1}, {next}
		loop_end

		;set to pref size
		method_call window, pref_size, {window}, {width, height}
		static_call window, change, {window, 0, 0, 640, height}

		;set window owner
		static_call sys_task, tcb, {}, {owner}
		static_call window, set_owner, {window, owner}

		;add to screen and dirty
		static_call gui_gui, add, {window}
		static_call window, dirty_all, {window}

		;app event loop
		loop_start
			static_call sys_mail, mymail, {}, {msg}

			;dispatch event to view
			method_call view, event, {msg->ev_data_view, msg}

			;if key event, then input to terminal
			if {msg->ev_data_type == ev_type_key && msg->ev_data_keycode > 0}
				assign {msg->ev_data_key}, {keychar}
				if {keychar == 13}
					;scroll lines
					static_call flow, get_first, {panel}, {label}
					static_call label, add_back, {label, panel}
					method_call flow, layout, {panel}
					static_call string, create, {">"}, {string}
					static_call label, set_text, {label, string}
					static_call flow, dirty_all, {panel}
				else
					;append char
					static_call flow, get_last, {panel}, {label}
					static_call string, create, {&keychar}, {string}
					static_call label, get_text, {label}, {line_string}
					static_call string, add, {line_string, string}, {new_line_string}
					static_call string, deref, {line_string}
					static_call string, deref, {string}
					static_call label, set_text, {label, new_line_string}
					static_call label, dirty, {label}
				endif
			endif

			;free event message
			static_call sys_mem, free, {msg}
		loop_end

		;deref window
		static_call window, deref, {window}
		method_call obj, deinit, {&myapp}
		pop_scope
		vp_ret

	line_list:
		%rep 37
			db '>', 0
		%endrep
		db '>Forth Terminal', 0
		db '>(C) C.A.Hinsley 2016', 0
		db '>', 0
		db 0

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
		long header_llink
		long header_hlink
		ubyte header_nsize
		struct header_name, null
	def_structure_end

	;format of header backwards from code entry point
	def_structure xt
		long xt_offset
		long xt_compile
		long xt_length
		long xt_body
	def_structure_end

	%macro defword 4
		%push newword
		%strlen %%l %1
		align long_size
	dic_%3:
		dq 0				; LATEST list link
		dq 0				; hash chain link
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

;;;;;;;;;;;;;;;;
; inlining words
;;;;;;;;;;;;;;;;

	defword "call,", 0, word_call_comma, word_call_comma
		vp_ret
	defword_end

	defword "inline,", 0, word_inline_comma, word_call_comma
		vp_ret
	defword_end

	fn_function_end
