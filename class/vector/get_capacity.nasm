%include 'inc/func.inc'
%include 'class/class_vector.inc'

	def_func class/vector/get_capacity
		;inputs
		;r0 = vector object
		;outputs
		;r0 = vector object
		;r1 = vector capacity

		vp_cpy [r0 + vector_capacity], r1
		vp_ret

	def_func_end
