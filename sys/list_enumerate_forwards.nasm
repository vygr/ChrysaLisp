%include 'inc/func.inc'
%include 'inc/list.inc'

	fn_function "sys/list_enumerate_forwards"
		;inputs
		;r0 = list head
		;r1 = user callback
		;r2 = user data pointer
		;outputs
		;r0 = list head
		;r1 = user callback
		;r2 = user data pointer
		;r5 = status
		;trashes
		;r3, r6
			;callback
			;inputs
			;r0 = list head
			;r1 = user callback
			;r2 = user data pointer
			;r3 = list node
			;outputs
			;r0 = list head
			;r1 = user callback
			;r2 = user data pointer
			;r5 = status
			;can trash
			;r3, r7-r14

		vp_xor r5, r5
		loop_list_forwards r0, r6, r3
			vp_call r1
		loop_until r5, !=, 0
		vp_ret

	fn_function_end
