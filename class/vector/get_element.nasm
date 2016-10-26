%include 'inc/func.inc'
%include 'class/class_vector.inc'

	def_func class/vector/get_element
		;inputs
		;r0 = vector object
		;r1 = vector element
		;outputs
		;r0 = vector object
		;r1 = object

		vp_mul ptr_size, r1
		vp_add [r0 + vector_array], r1
		vp_cpy [r1], r1
		vp_ret

	def_func_end
