%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'inc/load.inc'
%include 'class/class_string.inc'

	def_function class/string/create_from_long
		;inputs
		;r0 = number
		;r1 = base
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;all but r4

		const char_minus, "-"

		ptr this
		pubyte buffer, relloc
		long num, base

		push_scope
		retire {r0, r1}, {num, base}

		slot_function sys_load, statics
		assign {@_function_.ld_statics_reloc_buffer}, {relloc}
		assign {relloc}, {buffer}
		if {num < 0}
			assign {char_minus}, {*buffer}
			assign {buffer + 1}, {buffer}
			assign {-num}, {num}
		endif
		static_call sys_string, from_long, {num, buffer, base}
		static_call string, create_from_cstr, {relloc}, {this}

		eval {this}, {r0}
		pop_scope
		return

	def_function_end
