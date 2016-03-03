%include 'class/class_view.inc'

	fn_function class/test/oops

		;create a view object
		fn_debug entry_point
		class_call view, create
		if r0, !=, 0
			;deref it
			fn_debug calling_deref
			method_call view, deref
		endif
		fn_debug exit_point
		ret

	fn_function_end
