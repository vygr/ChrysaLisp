%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/font.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'
%include 'class/class_vector.inc'

	fn_function class/text/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		ptr inst
		ptr ctx
		ptr txt
		pptr words
		ulong index
		ulong length
		long x

		;save inputs
		push_scope
		retire {r0, r1}, {inst, ctx}

		;draw text
		if {inst->text_string && inst->text_font}
			assign {inst->text_words->vector_array}, {words}
			static_call vector, get_length, {inst->text_words}, {length}
			assign {0, 0}, {index, x}
			loop_start
				static_call gui_font, text, {inst->text_font, &words[index * ptr_size]->string_data}, {txt}
				if {txt}
					static_call gui_ctx, blit, {ctx, txt->ft_text_texture, inst->text_text_color, \
												x, 0, txt->ft_text_width, txt->ft_text_height}
					assign {x + txt->ft_text_width + (txt->ft_text_height >> 2)}, {x}
				endif
				assign {index + 1}, {index}
			loop_until {index == length}
		endif

		eval {inst}, {r0}
		pop_scope
		return

	fn_function_end
