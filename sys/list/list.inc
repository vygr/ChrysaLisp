(import 'sys/class.inc)
(import 'sys/code.inc)

;;;;;;;;;;;;;;;;;
; list structures
;;;;;;;;;;;;;;;;;

(def-struct 'lh_list)
	(ptr 'head)
	(ptr 'tail)
	(ptr 'tailpred)
(def-struct-end)

(def-struct 'ln_node)
	(ptr 'succ)
	(ptr 'pred)
(def-struct-end)

(def-struct 'ln_fnode)
	(ptr 'next)
(def-struct-end)

;;;;;;;;;;;;;;;;;;
; list node macros
;;;;;;;;;;;;;;;;;;

(defcfun ln-get-forward (_1 _2)
	;inputs
	;_1 = list node
	;_2 = temp
	;outputs
	;_1 = list node
	;trashes
	;_2

	(ln-get-succ _1 0 _2)
	(vpif `(,_2 == 0))
		(vp-cpy-ir _1 (neg lh_list_tail) _1)
	(endif))

(defcfun ln-get-backward (_1 _2)
	;inputs
	;_1 = list node
	;_2 = temp
	;outputs
	;_1 = list node
	;trashes
	;_2

	(ln-get-pred _1 0 _2)
	(vpif `(,_2 == 0))
		(ln-get-tail _1 _1)
	(endif))

(defcfun ln-set-succ (_1 _2)
	;inputs
	;_1 = list node1
	;_2 = list node2
	;outputs
	;_1 = list node1
	;_2 = list node2

	(vp-cpy-ri _2 _1 ln_node_succ))

(defcfun ln-set-pred (_1 _2)
	;inputs
	;_1 = list node1
	;_2 = list node2
	;outputs
	;_1 = list node1
	;_2 = list node2

	(vp-cpy-ri _2 _1 ln_node_pred))

(defcfun ln-get-succ (_1 _2 _3)
	;inputs
	;_1 = list node
	;_2 = offset
	;_3 = temp
	;outputs
	;_1 = list node
	;_3 = succ

	(vp-cpy-ir _1 (add ln_node_succ _2) _3))

(defcfun ln-get-pred (_1 _2 _3)
	;inputs
	;_1 = list node
	;_2 = offset
	;_3 = temp
	;outputs
	;_1 = list node
	;_3 = pred

	(vp-cpy-ir _1 (add ln_node_pred _2) _3))

(defcfun ln-add-node-after (_1 _2 _3)
	;inputs
	;_1 = list node1
	;_2 = list node2
	;_3 = temp
	;outputs
	;_1 = list node1
	;_2 = list node2
	;_3 = list node1 succ

	(ln-get-succ _1 0 _3)
	(ln-set-succ _1 _2)
	(ln-set-pred _3 _2)
	(ln-set-succ _2 _3)
	(ln-set-pred _2 _1))

(defcfun ln-add-node-before (_1 _2 _3)
	;inputs
	;_1 = list node1
	;_2 = list node2
	;_3 = temp
	;outputs
	;_1 = list node1
	;_2 = list node2
	;_3 = list node1 pred

	(ln-get-pred _1 0 _3)
	(ln-set-succ _2 _1)
	(ln-set-pred _2 _3)
	(ln-set-succ _3 _2)
	(ln-set-pred _1 _2))

(defcfun ln-remove-node (_1 _2)
	;inputs
	;_1 = list node
	;_2 = temp
	;outputs
	;_1 = list node pred
	;_2 = list node succ

	(ln-get-succ _1 0 _2)
	(ln-get-pred _1 0 _1)
	(ln-set-pred _2 _1)
	(ln-set-succ _1 _2))

(defcfun ln-is-first (_1 _2 _3)
	;inputs
	;_1 = list node
	;_2 = offset
	;_3 = temp
	;outputs
	;_1 = list node
	;_3 = 0 if first, else not

	(ln-get-pred _1 _2 _3)
	(ln-get-pred _3 0 _3))

(defcfun ln-is-last (_1 _2 _3)
	;inputs
	;_1 = list node
	;_2 = offset
	;_3 = temp
	;outputs
	;_1 = list node
	;_3 = 0 if last, else not

	(ln-get-succ _1 _2 _3)
	(ln-get-succ _3 0 _3))

;;;;;;;;;;;;;;;;;;;;
; list header macros
;;;;;;;;;;;;;;;;;;;;

(defcfun lh-list-object (l)
	(vp-align ptr_size)
(vp-label l)
	(times lh_list_size (vp-byte 0)))

(defcfun lh-init (_1 _2)
	;inputs
	;_1 = list head
	;_2 = temp
	;outputs
	;_1 = list head
	;_2 = list tail

	(vp-xor-rr _2 _2)
	(vp-cpy-ri _2 _1 lh_list_tail)
	(lh-set-tail _1 _1)
	(vp-lea-i _1 lh_list_tail _2)
	(lh-set-head _1 _2))

(defcfun lh-set-head (_1 _2)
	;inputs
	;_1 = list head
	;_2 = list node
	;outputs
	;_1 = list head
	;_2 = list node

	(vp-cpy-ri _2 _1 lh_list_head))

(defcfun lh-set-tail (_1 _2)
	;inputs
	;_1 = list head
	;_2 = list node
	;outputs
	;_1 = list head
	;_2 = list node

	(vp-cpy-ri _2 _1 lh_list_tailpred))

(defcfun lh-get-head (_1 _2 _3)
	;inputs
	;_1 = list head
	;_2 = offset
	;_3 = temp
	;outputs
	;_1 = list head
	;_3 = list node

	(vp-cpy-ir _1 (add lh_list_head _2) _3))

(defcfun lh-get-tail (_1 _2 _3)
	;inputs
	;_1 = list head
	;_2 = offset
	;_3 = temp
	;outputs
	;_1 = list head
	;_3 = list node

	(vp-cpy-ir _1 (add lh_list_tailpred _2) _3))

(defcfun lh-add-at-head (_1 _2 _3)
	;inputs
	;_1 = list head
	;_2 = list node
	;_3 = temp
	;outputs
	;_1 = list head
	;_2 = list node
	;_3 = list node succ

	(ln-add-node-after _1 _2 _3))

(defcfun lh-add-at-tail (_1 _2 _3)
	;inputs
	;_1 = list head
	;_2 = list node
	;_3 = temp
	;outputs
	;_1 = list head
	;_2 = list node
	;_3 = list node pred

	(vp-lea-i _1 lh_list_tail _1)
	(ln-add-node-before _1 _2 _3)
	(vp-lea-i _1 (neg lh_list_tail) _1))

(defcfun lh-remove-head (_1 _2 _3)
	;inputs
	;_1 = list head
	;_2 = temp
	;_3 = temp
	;outputs
	;_1 = list tail
	;_2 = 0
	;_3 = temp
	;else
	;_1 = list node pred
	;_2 = list node
	;_3 = list node succ

	(lh-get-head _1 0 _1)
	(ln-get-succ _1 0 _2)
	(vpif `(,_2 != 0))
		(vp-cpy-rr _1 _2)
		(ln-remove-node _1 _3)
	(endif))

(defcfun lh-remove-tail (_1 _2 _3)
	;inputs
	;_1 = list head
	;_2 = temp
	;_3 = temp
	;outputs
	;_1 = list tailpred
	;_2 = 0
	;_3 = temp
	;else
	;_1 = list node pred
	;_2 = list node
	;_3 = list node succ

	(lh-get-tail _1 0 _1)
	(ln-get-pred _1 0 _2)
	(vpif `(,_2 != 0))
		(vp-cpy-rr _1 _2)
		(ln-remove-node _1 _3)
	(endif))

(defcfun lh-is-empty (_1 _2 _3)
	;inputs
	;_1 = list head
	;_2 = offset
	;_3 = temp
	;outputs
	;_1 = list head
	;_3 = 0 if empty, else not

	(ln-get-succ _1 _2 _3)
	(ln-get-succ _3 0 _3))

;;;;;;;;;;;;;;;;;;
; list loop macros
;;;;;;;;;;;;;;;;;;

(defcfun loop-list-forward (_1 _2 _3 _4)
	;inputs
	;_1 = list head
	;_2 = offset
	;_3 = list node
	;_4 = list node look ahead
	;outputs
	;_1 = list head
	;_3 = list node
	;_4 = list node look ahead

	(lh-get-head _1 _2 _4)
	(loop-start)
		(vp-cpy-rr _4 _3)
		(ln-get-succ _4 0 _4)
		(breakif `(,_4 == 0)))

(defcfun loop-list-backward (_1 _2 _3 _4)
	;inputs
	;_1 = list head
	;_2 = offset
	;_3 = list node
	;_4 = list node look ahead
	;outputs
	;_1 = list head
	;_3 = list node
	;_4 = list node look ahead

	(lh-get-tail _1 _2 _4)
	(loop-start)
		(vp-cpy-rr _4 _3)
		(ln-get-pred _4 0 _4)
		(breakif `(,_4 == 0)))

(defcfun loop-flist (_1 _2 _3 _4)
	;inputs
	;_1 = list head
	;_2 = offset
	;_3 = list node
	;_4 = list last
	;outputs
	;_1 = list head
	;_3 = list node
	;_4 = list last

	(vp-lea-i _1 _2 _3)
	(loop-start)
		(ln-next-fnode _3 _4))

;;;;;;;;;;;;;;;;;;;;;
; forward list macros
;;;;;;;;;;;;;;;;;;;;;

(defcfun ln-next-fnode (_1 _2)
	;inputs
	;_1 = node
	;_2 = last
	;trashes
	;_1, _2

	(vp-cpy-rr _1 _2)
	(vp-cpy-ir _1 ln_fnode_next _1)
	(breakif `(,_1 == 0)))

(defcfun ln-remove-fnode (_1 _2)
	;inputs
	;_1 = node
	;_2 = last
	;trashes
	;_1

	(vp-cpy-ir _1 ln_fnode_next _1)
	(vp-cpy-ri _1 _2 ln_fnode_next)
	(vp-cpy-rr _2 _1))

(defcfun ln-add-fnode (_1 _2 _3 _4)
	;inputs
	;_1 = list
	;_2 = offset
	;_3 = node
	;_4 = temp
	;trashes
	;_4

	(vp-cpy-ir _1 (add _2 ln_fnode_next) _4)
	(vp-cpy-ri _4 _3 ln_fnode_next)
	(vp-cpy-ri _3 _1 (add _2 ln_fnode_next)))
