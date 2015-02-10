%include "vp.inc"
%include "code.inc"
%include "syscall.inc"

;;;;;;;;;;;
; util code
;;;;;;;;;;;

	SECTION .text

print_num:
	;inputs
	;r0 = number
	;r1 = fd
	;trashes
	;r0-r3, r5

	vp_cpy 10, r3	;base
	vp_cpy r4, r5	;stack location
	repeat
		vp_xor r2, r2
		vp_div r3
		vp_push r2
	until r0, ==, 0
	repeat
		vp_pop r0
		vp_add '0', r0
		sys_write_char r1, r0
	until r5, ==, r4
	vp_ret

print_list_head:
	;inputs
	;r0 = list head

	vp_push r0
	vp_cpy [r4], r0
	vp_cpy [r0 + LH_LIST_HEAD], r0
	vp_cpy 1, r1
	vp_call print_num
	sys_write_char 1, 10
	vp_cpy [r4], r0
	vp_cpy [r0 + LH_LIST_TAIL], r0
	vp_cpy 1, r1
	vp_call print_num
	sys_write_char 1, 10
	vp_cpy [r4], r0
	vp_cpy [r0 + LH_LIST_TAILPRED], r0
	vp_cpy 1, r1
	call print_num
	sys_write_char 1, 10
	vp_pop r0
	vp_ret

print_list_node:
	;inputs
	;r0 = list node

	push_trashed
	vp_push r0
	vp_cpy [r4], r0
	vp_add LN_NODE_SUCC, r0
	vp_cpy 1, r1
	call print_num
	sys_write_char 1, '|'
	vp_cpy [r4], r0
	vp_cpy [r0 + LN_NODE_SUCC], r0
	vp_cpy 1, r1
	vp_call print_num
	sys_write_char 1, 10
	vp_cpy [r4], r0
	vp_add LN_NODE_PRED, r0
	vp_cpy 1, r1
	call print_num
	sys_write_char 1, '|'
	vp_cpy [r4], r0
	vp_cpy [r0 + LN_NODE_PRED], r0
	vp_cpy 1, r1
	vp_call print_num
	sys_write_char 1, 10
	vp_pop r0
	pop_trashed
	vp_ret

print_node_callback:
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

	vp_push r0
	vp_push r1
	vp_push r2
	vp_cpy r3, r0
	call print_list_node
	vp_pop r2
	vp_pop r1
	vp_pop r0
	vp_xor r5, r5
	vp_ret

read_line:
	;inputs
	;r0 = buffer address
	;r1 = buffer size
	;r2 = fd
	;outputs
	;r0 = chars read
	;trashes
	;r0, r1, r2, r3, r5

	vp_cpy r0, r3
	vp_cpy r0, r5
	vp_add r0, r1
	repeat
		breakif r5, e, r1
		sys_read_char r2
		vp_cpy r0l, byte[r5]
		vp_inc r5
	until r0, ==, 10
	vp_cpy r5, r0
	vp_sub r3, r0
	vp_ret
