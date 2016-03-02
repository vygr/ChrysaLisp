%include 'class/class_view.inc'

	fn_function 'class/test/oops'

		;create a view object
		function_call view, create

		;deref it
		method_call ref, deref
		ret

	fn_function_end
