%include 'class/view.inc'
%include 'class/view1.inc'

	%macro print_method_number 1
		%warning Method Num %1 is _method_%[%1]_num
	%endmacro

	fn_function 'class/test/oops'
		print_method_number view_add
		print_method_number view_sub
		print_method_number view1_mul
		print_method_number view1_add
		print_method_number view1_sub
	fn_function_end
