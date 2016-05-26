%include 'inc/func.inc'

	fn_function test

		term_buffer_size equ 120

		def_structure term
			pubyte term_bufp
			ptr term_panel
			struct term_buf, term_buffer
		def_structure_end

		ptr terminal
		ubyte char
		ptr buf

		push_scope
			assign {char}, {*terminal->term_bufp}
			assign {*terminal->term_bufp}, {buf}
		pop_scope
		vp_ret

	fn_function_end
