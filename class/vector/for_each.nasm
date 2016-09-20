%include 'inc/func.inc'
%include 'class/class_vector.inc'

	def_function class/vector/for_each
		;inputs
		;r0 = vector object
		;r1 = predicate function pointer
		;r2 = predicate data pointer
		;outputs
		;r0 = vector object
		;r1 = 0, else break iterator
		;trashes
		;all but r0, r4
			;callback predicate
			;inputs
			;r0 = element iterator
			;r1 = predicate data pointer
			;outputs
			;r1 = 0 if break, else not
			;trashes
			;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_predicate
			ptr local_predicate_data
			ptr local_next
			ptr local_end
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2
		set_dst [r4 + local_inst], [r4 + local_predicate], [r4 + local_predicate_data]
		map_src_to_dst

		;process elements
		vp_cpy [r0 + vector_length], r1
		vp_cpy [r0 + vector_array], r0
		vp_add r0, r1
		vp_cpy r1, [r4 + local_end]
		loop_while r0, !=, [r4 + local_end]
			vp_cpy r0, [r4 + local_next]
			vp_cpy [r4 + local_predicate_data], r1
			vp_call [r4 + local_predicate]
			vp_cpy [r4 + local_next], r0
			breakif r1, ==, 0
			vp_add ptr_size, r0
		loop_end

		;iterator of break point, else 0
		vp_cpy r0, r1
		vp_cpy [r4 + local_inst], r0
		vp_cpy [r0 + vector_array], r2
		vp_add [r0 + vector_length], r2
		if r1, ==, r2
			vp_xor r1, r1
		endif
		vp_add local_size, r4
		vp_ret

	def_function_end
