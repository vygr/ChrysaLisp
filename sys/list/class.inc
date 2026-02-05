(include "lib/asm/class.inc")
(include "./struct.inc")

(def-class :sys_list :nil
	(dec-method :init sys/list/init :static (:r0)))

;;;;;;;;;;;;;;;;;;
; list node macros
;;;;;;;;;;;;;;;;;;

(defun ln-set-succ (%0 %1 %2)
	;inputs
	;%0 = list node
	;%1 = offset
	;%2 = succ
	;outputs
	;%0 = list node
	;%2 = succ

	(vp-cpy-ri %2 %0 (+ +ln_node_succ %1)))

(defun ln-set-pred (%0 %1 %2)
	;inputs
	;%0 = list node
	;%1 = offset
	;%2 = pred
	;outputs
	;%0 = list node
	;%2 = pred

	(vp-cpy-ri %2 %0 (+ +ln_node_pred %1)))

(defun ln-get-succ (%0 %1 %2)
	;inputs
	;%0 = list node
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list node
	;%2 = succ

	(vp-cpy-ir %0 (+ +ln_node_succ %1) %2))

(defun ln-get-pred (%0 %1 %2)
	;inputs
	;%0 = list node
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list node
	;%2 = pred

	(vp-cpy-ir %0 (+ +ln_node_pred %1) %2))

(defun ln-add-node-after (%0 %1 %2)
	;inputs
	;%0 = list node1
	;%1 = list node2
	;%2 = temp
	;outputs
	;%0 = list node1
	;%1 = list node2
	;%2 = list node1 succ

	(ln-get-succ %0 0 %2)
	(ln-set-succ %0 0 %1)
	(ln-set-pred %2 0 %1)
	(ln-set-succ %1 0 %2)
	(ln-set-pred %1 0 %0))

(defun ln-add-node-before (%0 %1 %2)
	;inputs
	;%0 = list node1
	;%1 = list node2
	;%2 = temp
	;outputs
	;%0 = list node1
	;%1 = list node2
	;%2 = list node1 pred

	(ln-get-pred %0 0 %2)
	(ln-set-succ %1 0 %0)
	(ln-set-pred %1 0 %2)
	(ln-set-succ %2 0 %1)
	(ln-set-pred %0 0 %1))

(defun ln-remove-node (%0 %1)
	;inputs
	;%0 = list node
	;%1 = temp
	;outputs
	;%0 = list node pred
	;%1 = list node succ

	(ln-get-succ %0 0 %1)
	(ln-get-pred %0 0 %0)
	(ln-set-pred %1 0 %0)
	(ln-set-succ %0 0 %1))

(defun ln-replace-node (%0 %1 %2)
	;inputs
	;%0 = list node
	;%1 = new list node
	;%2 = temp
	;outputs
	;%0 = list node
	;%1 = new list node

	(ln-get-succ %0 0 %2)
	(ln-set-succ %1 0 %2)
	(ln-set-pred %2 0 %1)
	(ln-get-pred %0 0 %2)
	(ln-set-pred %1 0 %2)
	(ln-set-succ %2 0 %1))

(defun ln-is-first (%0 %1 %2)
	;inputs
	;%0 = list node
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list node
	;%2 = 0 if first, else not

	(ln-get-pred %0 %1 %2)
	(ln-get-pred %2 0 %2))

(defun ln-is-last (%0 %1 %2)
	;inputs
	;%0 = list node
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list node
	;%2 = 0 if last, else not

	(ln-get-succ %0 %1 %2)
	(ln-get-succ %2 0 %2))

;;;;;;;;;;;;;;;;;;;;
; list header macros
;;;;;;;;;;;;;;;;;;;;

(defun lh-list-object (l)
	(vp-align +ptr_size)
(vp-label l)
	(times +lh_list_size (vp-byte 0)))

(defun lh-set-head (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list head
	;%2 = list node

	(vp-cpy-ri %2 %0 (+ +lh_list_head %1)))

(defun lh-set-tail (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list head
	;%2 = list node

	(vp-cpy-ri %2 %0 (+ +lh_list_tailpred %1)))

(defun lh-get-head (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list head
	;%2 = list node

	(vp-cpy-ir %0 (+ +lh_list_head %1) %2))

(defun lh-get-tail (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list head
	;%2 = list node

	(vp-cpy-ir %0 (+ +lh_list_tailpred %1) %2))

(defun lh-init (%0 %1)
	;inputs
	;%0 = list head
	;%1 = temp
	;outputs
	;%0 = list head
	;%1 = list tail

	(vp-xor-rr %1 %1)
	(vp-cpy-ri %1 %0 +lh_list_tail)
	(lh-set-tail %0 0 %0)
	(vp-lea-i %0 +lh_list_tail %1)
	(lh-set-head %0 0 %1))

(defun lh-add-at-head (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = list node
	;%2 = temp
	;outputs
	;%0 = list head
	;%1 = list node
	;%2 = list node succ

	(ln-add-node-after %0 %1 %2))

(defun lh-add-at-tail (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = list node
	;%2 = temp
	;outputs
	;%0 = list head
	;%1 = list node
	;%2 = list node pred

	(vp-lea-i %0 +lh_list_tail %0)
	(ln-add-node-before %0 %1 %2)
	(vp-lea-i %0 (neg +lh_list_tail) %0))

(defun lh-remove-head (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = temp
	;%2 = temp
	;outputs
	;%0 = list tail
	;%1 = 0
	;%2 = temp
	;else
	;%0 = list node pred
	;%1 = list node
	;%2 = list node succ

	(lh-get-head %0 0 %0)
	(ln-get-succ %0 0 %1)
	(vpif `(,%1 /= ,0))
		(vp-cpy-rr %0 %1)
		(ln-remove-node %0 %2)
	(endif))

(defun lh-remove-tail (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = temp
	;%2 = temp
	;outputs
	;%0 = list tailpred
	;%1 = 0
	;%2 = temp
	;else
	;%0 = list node pred
	;%1 = list node
	;%2 = list node succ

	(lh-get-tail %0 0 %0)
	(ln-get-pred %0 0 %1)
	(vpif `(,%1 /= ,0))
		(vp-cpy-rr %0 %1)
		(ln-remove-node %0 %2)
	(endif))

(defun lh-is-empty (%0 %1 %2)
	;inputs
	;%0 = list head
	;%1 = offset
	;%2 = temp
	;outputs
	;%0 = list head
	;%2 = 0 if empty, else not

	(lh-get-head %0 %1 %2)
	(ln-get-succ %2 0 %2))

;;;;;;;;;;;;;;;;;;;;;
; forward list macros
;;;;;;;;;;;;;;;;;;;;;

(defun ln-next-fnode (%0 %1)
	;inputs
	;%0 = node
	;%1 = last
	;trashes
	;%0, %1

	(vp-cpy-rr %0 %1)
	(vp-cpy-ir %0 +ln_fnode_next %0)
	(breakif `(,%0 = ,0)))

(defun ln-remove-fnode (%0 %1)
	;inputs
	;%0 = node
	;%1 = last
	;outputs
	;%1 = last
	;trashes
	;%0

	(vp-cpy-ir %0 +ln_fnode_next %0)
	(vp-cpy-ri %0 %1 +ln_fnode_next)
	(vp-cpy-rr %1 %0))

(defun ln-add-fnode (%0 %1 %2 %3)
	;inputs
	;%0 = list
	;%1 = offset
	;%2 = node
	;%3 = temp
	;trashes
	;%3

	(vp-cpy-ir %0 (+ %1 +ln_fnode_next) %3)
	(vp-cpy-ri %3 %2 +ln_fnode_next)
	(vp-cpy-ri %2 %0 (+ %1 +ln_fnode_next)))

;;;;;;;;;;;;;;;;;;
; list loop macros
;;;;;;;;;;;;;;;;;;

(defun loop-list-forward (%0 %1 %2 %3 &optional %4)
	;inputs
	;%0 = list head
	;%1 = offset
	;%2 = list node
	;%3 = list node look ahead
	;%4 = loop label
	;outputs
	;%0 = list head
	;%2 = list node
	;%3 = list node look ahead

	(lh-get-head %0 %1 %3)
	(loop-start %4)
		(vp-cpy-rr %3 %2)
		(ln-get-succ %3 0 %3)
		(breakif `(,%3 = ,0)))

(defun loop-list-backward (%0 %1 %2 %3 &optional %4)
	;inputs
	;%0 = list head
	;%1 = offset
	;%2 = list node
	;%3 = list node look ahead
	;%4 = loop label
	;outputs
	;%0 = list head
	;%2 = list node
	;%3 = list node look ahead

	(lh-get-tail %0 %1 %3)
	(loop-start %4)
		(vp-cpy-rr %3 %2)
		(ln-get-pred %3 0 %3)
		(breakif `(,%3 = ,0)))

(defun loop-flist (%0 %1 %2 %3 &optional %4)
	;inputs
	;%0 = list head
	;%1 = offset
	;%2 = list node
	;%3 = list last
	;%4 = loop label
	;outputs
	;%0 = list head
	;%2 = list node
	;%3 = list last

	(vp-lea-i %0 (+ %1 +ln_fnode_next) %2)
	(loop-start %4)
		(ln-next-fnode %2 %3))

(defun sys/list/init ()
	;inputs
	;:r0 = list header (ptr)
	(lh-init :r0 :r1))
