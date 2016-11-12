%include 'inc/func.ninc'
%include 'inc/gui.ninc'
%include 'inc/font.ninc'
%include 'class/class_text.ninc'
%include 'class/class_string.ninc'
%include 'class/class_vector.ninc'

def_func class/text/draw
	;inputs
	;r0 = view object
	;r1 = ctx object
	;trashes
	;all but r0, r4

	ptr inst, ctx, txt
	pptr words, words_end
	ulong length
	long x

	;save inputs
	push_scope
	retire {r0, r1}, {inst, ctx}

	;draw text
	if {inst->text_string && inst->text_font}
		assign {inst->text_words->vector_array}, {words}
		devirt_call vector, get_length, {inst->text_words}, {length}
		assign {&words[length * ptr_size]}, {words_end}
		assign {0}, {x}
		loop_start
			func_call gui_font, text, {inst->text_font, *words}, {txt}
			if {txt}
				func_call gui_ctx, blit, {ctx, txt->ft_text_texture, inst->text_text_color, \
											x, 0, txt->ft_text_width, txt->ft_text_height}
				assign {x + txt->ft_text_width + (txt->ft_text_height >> 2)}, {x}
			endif
			assign {words + ptr_size}, {words}
		loop_until {words == words_end}
	endif

	eval {inst}, {r0}
	pop_scope
	return

def_func_end
