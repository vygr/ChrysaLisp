%include "list.inc"

;;;;;;;;;;;
; list code
;;;;;;;;;;;

	SECTION .text

lh_enumerate_forwards:
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
		;r3, r7-r15

	vp_xor r5, r5
	lh_get_head r0, r6
	repeat
		vp_cpy r6, r3
		ln_get_succ r6, r6
		breakif r6, e, 0
		vp_call r1
	until r5, !=, 0
	vp_ret

lh_enumerate_backwards:
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
		;r3, r7-r15

	vp_xor r5, r5
	lh_get_tail r0, r6
	repeat
		vp_cpy r6, r3
		ln_get_pred r6, r6
		breakif r6, e, 0
		vp_call r1
	until r5, !=, 0
	vp_ret

lh_get_node_at_index:
	;inputs
	;r0 = list head
	;r1 = index
	;outputs
	;r0 = 0, else list node
	;trashes
	;r1, r2

	lh_get_head r0, r2
	loopstart
		vp_cpy r2, r0
		ln_get_succ r2, r2
		if r2, ==, 0
			vp_xor r0, r0
			vp_ret
		endif
		breakif r1, ==, 0
		vp_dec r1
	loopend
	vp_ret

lh_get_index_of_node:
	;inputs
	;r0 = list head
	;r1 = list node
	;outputs
	;r0 = -1, else index
	;r1 = list node
	;trashes
	;r2, r3

	lh_get_head r0, r2
	vp_xor r0, r0
	loopstart
		vp_cpy r2, r3
		ln_get_succ r2, r2
		if r2, ==, 0
			vp_cpy -1, r0
			vp_ret
		endif
		breakif r3, ==, r1
		vp_inc r0
	loopend
	vp_ret
