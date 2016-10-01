%include 'inc/func.inc'
%include 'inc/string.inc'
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
		pubyte buffer
		long num, base

		push_scope
		retire {r0, r1}, {num, base}

		assign {$buffer}, {buffer}
		if {num < 0}
			assign {char_minus}, {*buffer}
			assign {buffer + 1}, {buffer}
			assign {-num}, {num}
		endif
		static_call sys_string, from_long, {num, buffer, base}
		static_call string, create_from_cstr, {$buffer}, {this}

		eval {this}, {r0}
		pop_scope
		return

	buffer:
		;static buffer for number output
		;bad idea, but we are co-op sheduled so don't yield during this !!
		times 22 db 0

	def_function_end
