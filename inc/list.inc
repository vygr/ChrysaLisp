%ifndef LIST_1234
%define LIST_1234

%include 'inc/vp.inc'
%include 'inc/code.inc'

;;;;;;;;;;;;;;;;;;;
; list static class
;;;;;;;;;;;;;;;;;;;

	def_class list
	def_method forward, sys/list_forward
	def_method backward, sys/list_backward
	def_method index_of, sys/list_index_of
	def_method node_at, sys/list_node_at

;;;;;;;;;;;;;;;;;
; list structures
;;;;;;;;;;;;;;;;;

	def_structure	lh_list
		def_long	lh_list_head
		def_long	lh_list_tail
		def_long	lh_list_tailpred
	def_structure_end

	def_structure	ln_node
		def_long	ln_node_succ
		def_long	ln_node_pred
	def_structure_end

;;;;;;;;;;;;;;;;;;
; list node macros
;;;;;;;;;;;;;;;;;;

	%macro ln_get_forward 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;trashes
		;%2

		ln_get_succ %1, %2
		if %2, ==, 0
			vp_cpy [%1 - lh_list_tail], %1
		endif
	%endmacro

	%macro ln_get_backward 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;trashes
		;%2

		ln_get_pred %1, %2
		if %2, ==, 0
			ln_get_tail %1, %1
		endif
	%endmacro

	%macro ln_set_object 2
		;inputs
		;%1 = list node
		;%2 = object
		;outputs
		;%1 = list node
		;%2 = object

		vp_cpy %2, [%1 + ln_node_obj]
	%endmacro

	%macro ln_get_object 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;%2 = object

		vp_cpy [%1 + ln_node_obj], %2
	%endmacro

	%macro ln_set_succ 2
		;inputs
		;%1 = list node1
		;%2 = list node2
		;outputs
		;%1 = list node1
		;%2 = list node2

		vp_cpy %2, [%1 + ln_node_succ]
	%endmacro

	%macro ln_set_pred 2
		;inputs
		;%1 = list node1
		;%2 = list node2
		;outputs
		;%1 = list node1
		;%2 = list node2

		vp_cpy %2, [%1 + ln_node_pred]
	%endmacro

	%macro ln_get_succ 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;%2 = succ

		vp_cpy [%1 + ln_node_succ], %2
	%endmacro

	%macro ln_get_pred 2
		;inputs
		;%1 = list node
		;%2 = temp
		;outputs
		;%1 = list node
		;%2 = pred

		vp_cpy [%1 + ln_node_pred], %2
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
		times lh_list_size db 0
	%endmacro

	%macro lh_init 2
		;inputs
		;%1 = list head
		;%2 = temp
		;outputs
		;%1 = list head
		;%2 = list tail

		vp_cpy 0, qword[%1 + lh_list_tail]
		lh_set_tail %1, %1
		vp_lea [%1 + lh_list_tail], %2
		lh_set_head %1, %2
	%endmacro

	%macro lh_set_head 2
		;inputs
		;%1 = list head
		;%2 = list node
		;outputs
		;%1 = list head
		;%2 = list node

		vp_cpy %2, [%1 + lh_list_head]
	%endmacro

	%macro lh_set_tail 2
		;inputs
		;%1 = list head
		;%2 = list node
		;outputs
		;%1 = list head
		;%2 = list node

		vp_cpy %2, [%1 + lh_list_tailpred]
	%endmacro

	%macro lh_get_head 2
		;inputs
		;%1 = list head
		;%2 = temp
		;outputs
		;%1 = list head
		;%2 = list node

		vp_cpy [%1 + lh_list_head], %2
	%endmacro

	%macro lh_get_tail 2
		;inputs
		;%1 = list head
		;%2 = temp
		;outputs
		;%1 = list head
		;%2 = list node

		vp_cpy [%1 + lh_list_tailpred], %2
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

		vp_lea [%1 + lh_list_tail], %1
		ln_add_node_before %1, %2, %3
		vp_lea [%1 - lh_list_tail], %1
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
		if %2, !=, 0
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
		if %2, !=, 0
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

;;;;;;;;;;;;;;;;;;
; list loop macros
;;;;;;;;;;;;;;;;;;

	%macro loop_list_forwards 3
		;inputs
		;%1 = list head
		;%2 = list node look ahead
		;%3 = list node
		;outputs
		;%1 = list head
		;%2 = list node look ahead
		;%3 = list node

		lh_get_head %1, %2
		loop_start
			vp_cpy %2, %3
			ln_get_succ %2, %2
			breakif %2, ==, 0
	%endmacro

	%macro loop_list_backwards 3
		;inputs
		;%1 = list head
		;%2 = list node look ahead
		;%3 = list node
		;outputs
		;%1 = list head
		;%2 = list node look ahead
		;%3 = list node

		lh_get_tail %1, %2
		loop_start
			vp_cpy %2, %3
			ln_get_pred %2, %2
			breakif %2, ==, 0
	%endmacro

%endif
