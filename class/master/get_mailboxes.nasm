%include 'inc/func.inc'
%include 'class/class_master.inc'

	fn_function class/master/get_mailboxes
		;inputs
		;r0 = master object
		;outputs
		;r0 = master object
		;r1 = output mailbox address
		;r2 = error mailbox address

		vp_lea [r0 + master_output_mailbox], r1
		vp_lea [r0 + master_error_mailbox], r2
		vp_ret

	fn_function_end
