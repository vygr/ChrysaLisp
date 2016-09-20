%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'
%include 'class/class_vector.inc'

	def_function class/text/deinit
		;inputs
		;r0 = text object
		;trashes
		;all but r0, r4

		;save object
		vp_push r0

		;deref any string and words objects
		vp_cpy [r0 + text_string], r0
		if r0, !=, 0
			s_call string, deref, {r0}
			vp_cpy [r4], r0
			s_call vector, deref, {[r0 + text_words]}
		endif

		;deinit parent
		vp_pop r0
		p_jmp text, deinit, {r0}

	def_function_end
