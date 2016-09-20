%include 'inc/func.inc'
%include 'class/class_obj.inc'

	def_function class/obj/hash
		;inputs
		;r0 = object
		;outputs
		;r0 = object
		;r1 = hash code
		;trashes
		;all but r0, r4

		;save inputs
		vp_cpy r0, r1
		vp_ret

	def_function_end
