(import 'inc/vp.inc)
(import 'inc/code.inc)
;(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;;;
; list static class
;;;;;;;;;;;;;;;;;;;

;(def-class 'sys_list)
;(def-method 'init 'sys/list_init 'static '(r0))
;(def-method 'index_of 'sys/list_index_of 'static '(r0 r1) '(r1))
;(def-method 'node_at 'sys/list_node_at 'static '(r0 r1) '(r0))

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

(defcompilefun ln-get-forward (%1 %2)
	;inputs
	;%1 = list node
	;%2 = temp
	;outputs
	;%1 = list node
	;trashes
	;%2

	(ln-get-succ %1 0 %2)
	(vpif `(,%2 == 0))
		(vp-cpy-ir %1 (sub 0 lh_list_tail) %1)
	(endif))

(defcompilefun ln-get-backward (%1 %2)
	;inputs
	;%1 = list node
	;%2 = temp
	;outputs
	;%1 = list node
	;trashes
	;%2

	(ln-get-pred %1 0 %2)
	(vpif `(,%2 == 0))
		(ln-get-tail %1 %1)
	(endif))

(defcompilefun ln-set-succ (%1 %2)
	;inputs
	;%1 = list node1
	;%2 = list node2
	;outputs
	;%1 = list node1
	;%2 = list node2

	(vp-cpy-ri %2 %1 ln_node_succ))

(defcompilefun ln-set-pred (%1 %2)
	;inputs
	;%1 = list node1
	;%2 = list node2
	;outputs
	;%1 = list node1
	;%2 = list node2

	(vp-cpy-ri %2 %1 ln_node_pred))

(defcompilefun ln-get-succ (%1 %2 %3)
	;inputs
	;%1 = list node
	;%2 = offset
	;%3 = temp
	;outputs
	;%1 = list node
	;%3 = succ

	(vp-cpy-ir %1 (add ln_node_succ %2) %3))

(defcompilefun ln-get-pred (%1 %2 %3)
	;inputs
	;%1 = list node
	;%2 = offset
	;%3 = temp
	;outputs
	;%1 = list node
	;%3 = pred

	(vp-cpy-ir %1 (add ln_node_pred %2) %3))

(defcompilefun ln-add-node-after (%1 %2 %3)
	;inputs
	;%1 = list node1
	;%2 = list node2
	;%3 = temp
	;outputs
	;%1 = list node1
	;%2 = list node2
	;%3 = list node1 succ

	(ln-get-succ %1 0 %3)
	(ln-set-succ %1 %2)
	(ln-set-pred %3 %2)
	(ln-set-succ %2 %3)
	(ln-set-pred %2 %1))

(defcompilefun ln-add-node-before (%1 %2 %3)
	;inputs
	;%1 = list node1
	;%2 = list node2
	;%3 = temp
	;outputs
	;%1 = list node1
	;%2 = list node2
	;%3 = list node1 pred

	(ln-get-pred %1 0 %3)
	(ln-set-succ %2 %1)
	(ln-set-pred %2 %3)
	(ln-set-succ %3 %2)
	(ln-set-pred %1 %2))

(defcompilefun ln-remove-node (%1 %2)
	;inputs
	;%1 = list node
	;%2 = temp
	;outputs
	;%1 = list node pred
	;%2 = list node succ

	(ln-get-succ %1 0 %2)
	(ln-get-pred %1 0 %1)
	(ln-set-pred %2 %1)
	(ln-set-succ %1 %2))

(defcompilefun ln-is-first (%1 %2 %3)
	;inputs
	;%1 = list node
	;%2 = offset
	;%3 = temp
	;outputs
	;%1 = list node
	;%3 = 0 if first, else not

	(ln-get-pred %1 %2 %3)
	(ln-get-pred %3 0 %3))

(defcompilefun ln-is-last (%1 %2 %3)
	;inputs
	;%1 = list node
	;%2 = offset
	;%3 = temp
	;outputs
	;%1 = list node
	;%3 = 0 if last, else not

	(ln-get-succ %1 %2 %3)
	(ln-get-succ %3 0 %3))

;;;;;;;;;;;;;;;;;;;;
; list header macros
;;;;;;;;;;;;;;;;;;;;

(defcompilefun lh-list-object (l)
	(vp-align ptr_size)
(vp-label l)
	(times lh_list_size (vp-byte 0)))

(defcompilefun lh-init (%1 %2)
	;inputs
	;%1 = list head
	;%2 = temp
	;outputs
	;%1 = list head
	;%2 = list tail

	(vp-xor-rr %2 %2)
	(vp-cpy-ri %2 %1 lh_list_tail)
	(lh-set-tail %1 %1)
	(vp-lea-i %1 lh_list_tail %2)
	(lh-set-head %1 %2))

(defcompilefun lh-set-head (%1 %2)
	;inputs
	;%1 = list head
	;%2 = list node
	;outputs
	;%1 = list head
	;%2 = list node

	(vp-cpy-ri %2 %1 lh_list_head))

(defcompilefun lh-set-tail (%1 %2)
	;inputs
	;%1 = list head
	;%2 = list node
	;outputs
	;%1 = list head
	;%2 = list node

	(vp-cpy-ri %2 %1 lh_list_tailpred))

(defcompilefun lh-get-head (%1 %2 %3)
	;inputs
	;%1 = list head
	;%2 = offset
	;%3 = temp
	;outputs
	;%1 = list head
	;%3 = list node

	(vp-cpy-ir %1 (add lh_list_head %2) %3))

(defcompilefun lh-get-tail (%1 %2 %3)
	;inputs
	;%1 = list head
	;%2 = offset
	;%3 = temp
	;outputs
	;%1 = list head
	;%3 = list node

	(vp-cpy-ir %1 (add lh_list_tailpred %2) %3))

(defcompilefun lh-add-at-head (%1 %2 %3)
	;inputs
	;%1 = list head
	;%2 = list node
	;%3 = temp
	;outputs
	;%1 = list head
	;%2 = list node
	;%3 = list node succ

	(ln-add-node-after %1 %2 %3))

(defcompilefun lh-add-at-tail (%1 %2 %3)
	;inputs
	;%1 = list head
	;%2 = list node
	;%3 = temp
	;outputs
	;%1 = list head
	;%2 = list node
	;%3 = list node pred

	(vp-lea-i %1 lh_list_tail %1)
	(ln-add-node-before %1 %2 %3)
	(vp-lea-i %1 (sub 0 lh_list_tail) %1))

(defcompilefun lh-remove-head (%1 %2 %3)
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

	(lh-get-head %1 0 %1)
	(ln-get-succ %1 0 %2)
	(vpif `(,%2 != 0))
		(vp-cpy-rr %1 %2)
		(ln-remove-node %1 %3)
	(endif))

(defcompilefun lh-remove-tail (%1 %2 %3)
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

	(lh-get-tail %1 0 %1)
	(ln-get-pred %1 0 %2)
	(vpif `(,%2 != 0))
		(vp-cpy-rr %1 %2)
		(ln-remove-node %1 %3)
	(endif))

(defcompilefun lh-is-empty (%1 %2 %3)
	;inputs
	;%1 = list head
	;%2 = offset
	;%3 = temp
	;outputs
	;%1 = list head
	;%3 = 0 if empty, else not

	(ln-get-succ %1 %2 %3)
	(ln-get-succ %3 0 %3))

;;;;;;;;;;;;;;;;;;
; list loop macros
;;;;;;;;;;;;;;;;;;

(defcompilefun loop-list-forward (%1 %2 %3 %4)
	;inputs
	;%1 = list head
	;%2 = offset
	;%3 = list node
	;%4 = list node look ahead
	;outputs
	;%1 = list head
	;%3 = list node
	;%4 = list node look ahead

	(lh-get-head %1 %2 %4)
	(loop-start)
		(vp-cpy-rr %4 %3)
		(ln-get-succ %4 0 %4)
		(breakif `(,%4 == 0)))

(defcompilefun loop-list-backward (%1 %2 %3 %4)
	;inputs
	;%1 = list head
	;%2 = offset
	;%3 = list node
	;%4 = list node look ahead
	;outputs
	;%1 = list head
	;%3 = list node
	;%4 = list node look ahead

	(lh-get-tail %1 %2 %4)
	(loop-start)
		(vp-cpy-rr %4 %3)
		(ln-get-pred %4 0 %4)
		(breakif `(,%4 == 0)))

(defcompilefun loop-flist-forward (%1 %2 %3 %4)
	;inputs
	;%1 = list head
	;%2 = offset
	;%3 = list node
	;%4 = list last
	;outputs
	;%1 = list head
	;%3 = list node
	;%4 = list last

	(vp-lea-i %1 %2 %3)
	(loop-start)
		(ln-next-fnode %3 %4))

;;;;;;;;;;;;;;;;;;;;;
; forward list macros
;;;;;;;;;;;;;;;;;;;;;

(defcompilefun ln-next-fnode (%1 %2)
	;inputs
	;%1 = node
	;%2 = last
	;trashes
	;%1, %2

	(vp-cpy-rr %1 %2)
	(vp-cpy-ir %1 ln_fnode_next %1)
	(breakif `(,%1 == 0)))

(defcompilefun ln-remove-fnode (%1 %2)
	;inputs
	;%1 = node
	;%2 = last
	;trashes
	;%1

	(vp-cpy-ir %1 ln_fnode_next %1)
	(vp-cpy-ri %1 %2 ln_fnode_next)
	(vp-cpy-rr %2 %1))

(defcompilefun ln-add-fnode (%1 %2 %3 %4)
	;inputs
	;%1 = list
	;%2 = offset
	;%3 = node
	;%4 = temp
	;trashes
	;%4

	(vp-cpy-ir %1 (add %2 ln_fnode_next) %4)
	(vp-cpy-ri %4 %3 ln_fnode_next)
	(vp-cpy-ri %3 %1 (add %2 ln_fnode_next)))
