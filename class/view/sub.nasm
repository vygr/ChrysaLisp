%include 'inc/func.inc'
%include 'class/class_view.inc'

	def_function class/view/sub
		;inputs
		;r0 = object
		;trashes
		;r1-r2

		;test parent
		vp_cpy [r0 + view_parent], r1
		if r1, !=, 0
			;clear parent field
			vp_cpy_cl 0, [r0 + view_parent]

			;remove from parent list
			vp_lea [r0 + view_node], r2
			ln_remove_node r2, r1
		endif
		vp_ret

	def_function_end
