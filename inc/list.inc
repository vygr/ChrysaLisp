(import 'inc/vp.inc)
(import 'inc/code.inc)

;;;;;;;;;;;;;;;;;;;
; list static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_list)
(def-method 'init 'sys/list_init 'static '(r0))
(def-method 'index_of 'sys/list_index_of 'static '(r0 r1) '(r1))
(def-method 'node_at 'sys/list_node_at 'static '(r0 r1) '(r0))

;;;;;;;;;;;;;;;;;
; list structures
;;;;;;;;;;;;;;;;;

(def-struct 'lh_list)
	(ptr 'lh_list_head)
	(ptr 'lh_list_tail)
	(ptr 'lh_list_tailpred)
(def-struct-end)

(def-struct 'ln_node)
	(ptr 'ln_node_succ)
	(ptr 'ln_node_pred)
(def-struct-end)

(def-struct 'ln_fnode)
	(ptr 'ln_fnode_next)
(def-struct-end)

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

	vp_cpy_cl 0, [%1 + lh_list_tail]
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

%macro loop_list_forward 3
	;inputs
	;%1 = list head
	;%2 = list node
	;%3 = list node look ahead
	;outputs
	;%1 = list head
	;%2 = list node
	;%3 = list node look ahead

	lh_get_head %1, %3
	(loop-start)
		vp_cpy %3, %2
		ln_get_succ %3, %3
		breakif %3, ==, 0
%endmacro

%macro loop_list_backward 3
	;inputs
	;%1 = list head
	;%2 = list node
	;%3 = list node look ahead
	;outputs
	;%1 = list head
	;%2 = list node
	;%3 = list node look ahead

	lh_get_tail %1, %3
	(loop-start)
		vp_cpy %3, %2
		ln_get_pred %3, %3
		breakif %3, ==, 0
%endmacro

%macro loop_flist_forward 3
	;inputs
	;%1 = list head
	;%2 = list node
	;%3 = list last
	;outputs
	;%1 = list head
	;%2 = list node
	;%3 = list last

	%ifnidn %1, %2
		vp_lea [%1], %2
	%endif
	(loop-start)
		ln_next_fnode %2, %3
%endmacro

;;;;;;;;;;;;;;;;;;;;;
; forward list macros
;;;;;;;;;;;;;;;;;;;;;

%macro ln_next_fnode 2
	;inputs
	;%1 = node
	;%2 = last
	;trashes
	;%1, %2

	vp_cpy %1, %2
	vp_cpy [%1 + ln_fnode_next], %1
	breakif %1, ==, 0
%endmacro

%macro ln_remove_fnode 2
	;inputs
	;%1 = node
	;%2 = last
	;trashes
	;%1

	vp_cpy [%1 + ln_fnode_next], %1
	vp_cpy %1, [%2 + ln_fnode_next]
	vp_cpy %2, %1
%endmacro

%macro ln_add_fnode 3
	;inputs
	;%1 = list
	;%2 = node
	;%3 = temp
	;trashes
	;%3

	vp_cpy [%1 + ln_fnode_next], %3
	vp_cpy %3, [%2 + ln_fnode_next]
	vp_cpy %2, [%1 + ln_fnode_next]
%endmacro
