%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'
%include 'class/class_vector.inc'

	def_function class/text/pref_size
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
		if {inst->text_string && inst->text_font}
			assign {inst->text_words->vector_array}, {words}
			static_call vector, get_length, {inst->text_words}, {length}
			assign {&words[length * ptr_size]}, {words_end}
			loop_start
				static_call gui_font, bounds, {inst->text_font, &(*words)->string_data}, {word_w, height}
				assign {words + ptr_size}, {words}
				assign {width + word_w}, {width}
			loop_until {words == words_end}
			assign {width + (length - 1) * (height >> 2)}, {width}
		endif

		eval {inst, width, height}, {r0, r10, r11}
		pop_scope
		return

	def_function_end
