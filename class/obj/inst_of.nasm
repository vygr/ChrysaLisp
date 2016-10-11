%include 'inc/func.inc'
%include 'class/class_obj.inc'

	def_function class/obj/inst_of
		;inputs
		;r0 = object
		;r1 = vtable pointer
		;outputs
		;r0 = object
		;r1 = 0 if not, else vtable pointer
		;trashes
		;r2

		vp_cpy r1, r2
		vp_cpy [r0 + obj_vtable], r1
		loop_while r1, !=, r2
			vp_cpy [r1], r1
		loop_until r1, ==, 0
		vp_ret

	def_function_end
