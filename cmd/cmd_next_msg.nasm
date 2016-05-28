%include 'inc/func.inc'
%include 'cmd/cmd.inc'

	fn_function cmd/cmd_next_msg
		;inputs
		;r0 = list head
		;r1 = 0, else new msg
		;r2 = seqnum
		;outputs
		;r0 = 0, else next msg
		;trashes
		;r1-r3

		;save any new msg
		if r1, !=, 0
			lh_add_at_tail r0, r1, r3
		endif

		;scan for seqnum
		loop_list_forward r0, r0, r1
		loop_until r2, ==, [r0 + cmd_mail_seqnum]
		if r1, ==, 0
			;not found
			vp_cpy r1, r0
		else
			;remove found from list
			vp_cpy r0, r1
			ln_remove_node r1, r2
		endif
		vp_ret

	fn_function_end
