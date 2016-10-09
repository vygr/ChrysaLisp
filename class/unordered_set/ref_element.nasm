%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/ref_element
		;inputs
		;r0 = unordered_set object
		;r1 = element index
		;outputs
		;r0 = unordered_set object
		;r1 = element
		;trashes
		;all but r0, r4

		vp_push r0
		s_call unordered_set, get_element, {r0, r1}, {r1}
		s_call ref, ref, {r1}
		vp_cpy r0, r1
		vp_pop r0
		vp_ret

	def_function_end
