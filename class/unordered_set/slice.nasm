%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/slice
		;inputs
		;r0 = unordered_set object
		;r1 = start element
		;r2 = end element
		;outputs
		;r0 = unordered_set object
		;r1 = unordered_set slice
		;trashes
		;all but r0, r4

		;save inputs
		set_src r0, r1, r2
		set_dst r9, r10, r11
		map_src_to_dst

		s_call unordered_set, create, {[r0 + unordered_set_key_callback], 1}, {r1}
		s_jmp unordered_set, slice_impl, {r9, r1, r10, r11}

	def_function_end
