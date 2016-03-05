%include 'inc/func.inc'
%include 'inc/mail.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	ARRAY_SIZE equ 16

	fn_function tests/test7
		;task started by test1

		;allocate temp array for mailbox ID's
		vp_cpy 16*ARRAY_SIZE, r0
		class_call mem, alloc
		fn_assert r0, !=, 0
		vp_cpy r0, r14

		;open test8 array, off chip
		vp_cpy r14, r1
		vp_lea [rel task_eights], r0
		class_call task, array

		;send exit messages etc
		for r13, 0, ARRAY_SIZE, 1
			class_call mail, alloc
			fn_assert r0, !=, 0
			vp_cpy r13, r3
			vp_mul 16, r3
			vp_cpy [r14 + r3], r1
			vp_cpy [r14 + r3 + 8], r2
			vp_cpy r1, [r0 + ml_msg_dest]
			vp_cpy r2, [r0 + (ml_msg_dest + 8)]
			class_call mail, send
			class_call task, yield
		next

		;free ID array and return
		vp_cpy r14, r0
		class_jmp mem, free

	task_eights:
		%rep ARRAY_SIZE
		db 'tests/test8', 0
		%endrep
		db 0

	fn_function_end
