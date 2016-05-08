%include 'inc/func.inc'
%include 'inc/mail.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function 'test'

		;define variables
		pulong array
		push_scope

		set_token_list array[(30*8)].mb_cpu
		print_token_list
		token_to_rpn
		print_rpn_list

		set_reg_stack
		fill_reg_stack
		compile_rpn_list

		pop_scope
		vp_ret

	fn_function_end
