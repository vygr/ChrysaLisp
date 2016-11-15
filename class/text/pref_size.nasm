%include 'inc/func.ninc'
%include 'inc/font.ninc'
%include 'class/class_text.ninc'
%include 'class/class_string.ninc'
%include 'class/class_vector.ninc'

def_func class/text/pref_size
	;inputs
	;r0 = text object
	;outputs
	;r10 = prefered width
	;r11 = prefered height
	;trashes
	;all but r0, r4

	ptr inst
	pptr words, words_end
	ulong width, height, length, word_w

	;save inputs
	push_scope
	retire {r0}, {inst}

	;bounds of text
	assign {0, 0}, {width, height}
	vpif {inst->text_string && inst->text_font}
		assign {inst->text_words->vector_array}, {words}
		devirt_call vector, get_length, {inst->text_words}, {length}
		assign {&words[length * ptr_size]}, {words_end}
		loop_start
			func_call gui_font, bounds, {inst->text_font, *words}, {word_w, height}
			assign {words + ptr_size}, {words}
			assign {width + word_w}, {width}
		loop_until {words == words_end}
		assign {width + (length - 1) * (height >> 2)}, {width}
	endif

	expr {inst, width, height}, {r0, r10, r11}
	pop_scope
	return

def_func_end
