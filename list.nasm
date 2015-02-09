%ifndef LIST_1234
    %define LIST_1234

%include "vp.nasm"
%include "code.nasm"

;;;;;;;;;;;;;;;;;
; list structures
;;;;;;;;;;;;;;;;;

	struc LH_LIST
		LH_LIST_HEAD:		resq 1
		LH_LIST_TAIL:		resq 1
		LH_LIST_TAILPRED:	resq 1
		LH_LIST_SIZE:
	endstruc

	struc LN_NODE
		LN_NODE_SUCC:		resq 1
		LN_NODE_PRED:		resq 1
		LN_NODE_OBJ:		resq 1
		LN_NODE_SIZE:
	endstruc

;;;;;;;;;;;;;;;;;;
; list node macros
;;;;;;;;;;;;;;;;;;

	%macro ln_set_object 2
		;inputs
		;%1 = list node
		;%2 = object
		;outputs
		;%1 = list node
		;%2 = object

		vp_cpy %2, [%1 + LN_NODE_OBJ]
	%endmacro

	%macro ln_get_object 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;%2 = object

		vp_cpy [%1 + LN_NODE_OBJ], %2
	%endmacro

	%macro ln_set_succ 2
		;inputs
		;%1 = list node1
		;%2 = list node2
		;outputs
		;%1 = list node1
		;%2 = list node2

		vp_cpy %2, [%1 + LN_NODE_SUCC]
	%endmacro

	%macro ln_set_pred 2
		;inputs
		;%1 = list node1
		;%2 = list node2
		;outputs
		;%1 = list node1
		;%2 = list node2

		vp_cpy %2, [%1 + LN_NODE_PRED]
	%endmacro

	%macro ln_get_succ 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;%2 = succ

		vp_cpy [%1 + LN_NODE_SUCC], %2
	%endmacro

	%macro ln_get_pred 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;%2 = pred

		vp_cpy [%1 + LN_NODE_PRED], %2
	%endmacro

	%macro ln_add_node_after 3
		;inputs
		;%1 = list node1
		;%2 = list node2
		;%3 = temp
		;outputs
		;%1 = list node1
		;%2 = list node2
		;%3 = list node1 succ

		ln_get_succ %1, %3
		ln_set_succ %1, %2
		ln_set_pred %3, %2
		ln_set_succ %2, %3
		ln_set_pred %2, %1
	%endmacro

	%macro ln_add_node_before 3
		;inputs
		;%1 = list node1
		;%2 = list node2
		;%3 = temp
		;outputs
		;%1 = list node1
		;%2 = list node2
		;%3 = list node1 pred

		ln_get_pred %1, %3
		ln_set_succ %2, %1
		ln_set_pred %2, %3
		ln_set_succ %3, %2
		ln_set_pred %1, %2
	%endmacro

	%macro ln_remove_node 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node pred
		;%2 = list node succ

		ln_get_succ %1, %2
		ln_get_pred %1, %1
		ln_set_pred %2, %1
		ln_set_succ %1, %2
	%endmacro

	%macro ln_is_first 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;%2 = 0 if first, else not

		ln_get_pred %1, %2
		ln_get_pred %2, %2
	%endmacro

	%macro ln_is_last 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;%2 = 0 if last, else not

		ln_get_succ %1, %2
		ln_get_succ %2, %2
	%endmacro

;;;;;;;;;;;;;;;;;;;;
; list header macros
;;;;;;;;;;;;;;;;;;;;

	%macro lh_list_object 1
		align 8
	%1:
		times LH_LIST_SIZE db 0
	%endmacro

	%macro lh_init 2
		;inputs
		;%1 = list head
		;%2 = temp
		;outputs
		;%1 = list head
		;%2 = list tail

		vp_cpy 0, long[%1 + LH_LIST_TAIL]
		lh_set_tail %1, %1
		vp_lea [%1 + LH_LIST_TAIL], %2
		lh_set_head %1, %2
	%endmacro

	%macro lh_set_head 2
		;inputs
		;%1 = list head
		;%2 = list node
		;outputs
		;%1 = list head
		;%2 = list node

		vp_cpy %2, [%1 + LH_LIST_HEAD]
	%endmacro

	%macro lh_set_tail 2
		;inputs
		;%1 = list head
		;%2 = list node
		;outputs
		;%1 = list head
		;%2 = list node

		vp_cpy %2, [%1 + LH_LIST_TAILPRED]
	%endmacro

	%macro lh_get_head 2
		;inputs
		;%1 = list head
		;%2 = temp
		;outputs
		;%1 = list head
		;%2 = list node

		vp_cpy [%1 + LH_LIST_HEAD], %2
	%endmacro

	%macro lh_get_tail 2
		;inputs
		;%1 = list head
		;%2 = temp
		;outputs
		;%1 = list head
		;%2 = list node

		vp_cpy [%1 + LH_LIST_TAILPRED], %2
	%endmacro

	%macro lh_add_at_head 3
		;inputs
		;%1 = list head
		;%2 = list node
		;%3 = temp
		;outputs
		;%1 = list head
		;%2 = list node
		;%3 = list node succ

		ln_add_node_after %1, %2, %3
	%endmacro

	%macro lh_add_at_tail 3
		;inputs
		;%1 = list head
		;%2 = list node
		;%3 = temp
		;outputs
		;%1 = list head
		;%2 = list node
		;%3 = list node pred

		vp_lea [%1 + LH_LIST_TAIL], %1
		ln_add_node_before %1, %2, %3
		vp_lea [%1 - LH_LIST_TAIL], %1
	%endmacro

	%macro lh_remove_head 3
		;inputs
		;%1 = list head
		;%2 = temp
		;%3 = temp
		;outputs
		;%1 = list tail
		;%2 = 0
		;%3 = temp
		;else
		;%1 = list node pred
		;%2 = list node
		;%3 = list node succ

		lh_get_head %1, %1
		ln_get_succ %1, %2
		if %2, ne, 0
			vp_cpy %1, %2
			ln_remove_node %1, %3
		endif
	%endmacro

	%macro lh_remove_tail 3
		;inputs
		;%1 = list head
		;%2 = temp
		;%3 = temp
		;outputs
		;%1 = list tailpred
		;%2 = 0
		;%3 = temp
		;else
		;%1 = list node pred
		;%2 = list node
		;%3 = list node succ

		lh_get_tail %1, %1
		ln_get_pred %1, %2
		if %2, ne, 0
			vp_cpy %1, %2
			ln_remove_node %1, %3
		endif
	%endmacro

	%macro lh_is_empty 2
		;inputs
		;%1 = list head
		;%2 = temp
		;outputs
		;%1 = list head
		;%2 = 0 if empty, else not

		ln_get_succ %1,%2
		ln_get_succ %2,%2
	%endmacro

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
	until r5, ne, 0
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
	until r5, ne, 0
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
		if r2, e, 0
			vp_xor r0, r0
			vp_ret
		endif
		breakif r1, e, 0
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
		if r2, e, 0
			vp_cpy -1, r0
			vp_ret
		endif
		breakif r3, e, r1
		vp_inc r0
	loopend
	vp_ret

%endif
