%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/test/oops

		;create a view object
		class_call view, create
		if r0, !=, 0
			;deref it
			fn_debug calling_deref
			method_call view, deref
		endif
		vp_ret

	fn_function_end
