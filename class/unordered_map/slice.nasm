%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_vector.inc'

	def_func class/unordered_map/slice
		;inputs
		;r0 = unordered_map object
		;r1 = start element
		;r2 = end element
		;outputs
		;r0 = unordered_map object
		;r1 = unordered_map slice
		;trashes
		;all but r0, r4

		;save inputs
		set_src r0, r1, r2
		set_dst r9, r10, r11
		map_src_to_dst

		f_call unordered_map, create, {[r0 + unordered_set_key_callback], 1}, {r1}
		f_jmp unordered_map, slice_impl, {r9, r1, r10, r11}

	def_func_end
