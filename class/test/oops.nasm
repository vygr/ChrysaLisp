%include 'class/class_view.inc'

	fn_function 'class/test/oops'

		;create a view object
		class_call view, create
		if r0, !=, 0
			;deref it
			method_call ref, deref
		endif
		ret

	fn_function_end
