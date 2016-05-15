%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/to_front
		;inputs
		;r0 = view object
		;outputs
		;r0 = view object
		;trashes
		;r2

		;are we allready first ?
		s_call view, get_first, {r0}, {r2}
		s_call view, get_parent, {r0}, {r1}
		if r1, !=, r2
			;no so best jump forward then
			vp_push r1
			s_call view, sub, {r0}
			vp_pop r1
			s_call view, add, {r0, r1}
			s_jmp view, dirty_all, {r0}
		endif
		vp_ret

	fn_function_end
