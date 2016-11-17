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

(defun ln-get-forward (x y)
	;inputs
	;x = list node
	;y = temp
	;outputs
	;x = list node
	;trashes
	;y

	(ln-get-succ x y)
	(vpif '(y == 0))
		(vp-cpy-ir x (sub 0 lh_list_tail) x)
	(endif))

(defun ln-get-backward (x y)
	;inputs
	;x = list node
	;y = temp
	;outputs
	;x = list node
	;trashes
	;y

	(ln-get-pred x y)
	(vpif '(y == 0))
		(ln-get-tail x x)
	(endif))

(defun ln-set-succ (x y)
	;inputs
	;x = list node1
	;y = list node2
	;outputs
	;x = list node1
	;y = list node2

	(vp-cpy-ri y x ln-node-succ))

(defun ln-set-pred (x y)
	;inputs
	;x = list node1
	;y = list node2
	;outputs
	;x = list node1
	;y = list node2

	(vp-cpy-ri y x ln_node_pred))

(defun ln-get-succ (x y)
	;inputs
	;x = list node
	;y = temp
	;outputs
	;x = list node
	;y = succ

	(vp-cpy-ir x ln_node_succ y))

(defun ln-get-pred (x y)
	;inputs
	;x = list node
	;y = temp
	;outputs
	;x = list node
	;y = pred

	(vp-cpy-ir x ln_node_pred y))

(defun ln-add-node-after (x y z)
	;inputs
	;x = list node1
	;y = list node2
	;z = temp
	;outputs
	;x = list node1
	;y = list node2
	;z = list node1 succ

	(ln-get-succ x z)
	(ln-set-succ x y)
	(ln-set-pred z y)
	(ln-set-succ y z)
	(ln-set-pred y x))

(defun ln-add-node-before (x y z)
	;inputs
	;x = list node1
	;y = list node2
	;z = temp
	;outputs
	;x = list node1
	;y = list node2
	;z = list node1 pred

	(ln-get-pred x z)
	(ln-set-succ y x)
	(ln-set-pred y z)
	(ln-set-succ z y)
	(ln-set-pred x y))

(defun ln-remove-node (x y)
	;inputs
	;x = list node
	;y = temp
	;outputs
	;x = list node pred
	;y = list node succ

	(ln-get-succ x y)
	(ln-get-pred x x)
	(ln-set-pred y x)
	(ln-set-succ x y))

(defun ln-is-first (x y)
	;inputs
	;x = list node
	;y = temp
	;outputs
	;x = list node
	;y = 0 if first, else not

	(ln-get-pred x y)
	(ln-get-pred y y))

(defun ln-is-last (x y)
	;inputs
	;x = list node
	;y = temp
	;outputs
	;x = list node
	;y = 0 if last, else not

	(ln-get-succ x y)
	(ln-get-succ y y))

;;;;;;;;;;;;;;;;;;;;
; list header macros
;;;;;;;;;;;;;;;;;;;;

(defun lh-list-object (l)
	(vp-align ptr_size)
(vp-label l)
	(times lh_list_size (vp-byte 0)))

(defun lh-init (x y)
	;inputs
	;x = list head
	;y = temp
	;outputs
	;x = list head
	;y = list tail

	(vp-xor-rr y y)
	(vp-cpy-ri y x lh_list_tail)
	(lh-set-tail x x)
	(vp-lea-i x lh_list_tail y)
	(lh-set-head x y))

(defun lh-set-head (x y)
	;inputs
	;x = list head
	;y = list node
	;outputs
	;x = list head
	;y = list node

	(vp-cpy-ri y x lh_list_head))

(defun lh-set-tail (x y)
	;inputs
	;x = list head
	;y = list node
	;outputs
	;x = list head
	;y = list node

	(vp-cpy-ri y x lh_list_tailpred))

(defun lh-get-head (x y)
	;inputs
	;x = list head
	;y = temp
	;outputs
	;x = list head
	;y = list node

	(vp-cpy-ir x lh_list_head y))

(defun lh-get-tail (x y)
	;inputs
	;x = list head
	;y = temp
	;outputs
	;x = list head
	;y = list node

	(vp-cpy-ir x lh_list_tailpred y))

(defun lh-add-at-head (x y z)
	;inputs
	;x = list head
	;y = list node
	;z = temp
	;outputs
	;x = list head
	;y = list node
	;z = list node succ

	(ln-add-node-after x y z))

(defun lh-add-at-tail (x y z)
	;inputs
	;x = list head
	;y = list node
	;z = temp
	;outputs
	;x = list head
	;y = list node
	;z = list node pred

	(vp-lea-i x lh_list_tail x)
	(ln-add-node-before x y z)
	(vp-lea-i x (sub 0 lh_list_tail) x))

(defun lh-remove-head (x y z)
	;inputs
	;x = list head
	;y = temp
	;z = temp
	;outputs
	;x = list tail
	;y = 0
	;z = temp
	;else
	;x = list node pred
	;y = list node
	;z = list node succ

	(lh-get-head x x)
	(ln-get-succ x y)
	(vpif '(y != 0))
		(vp-cpy-rr x y)
		(ln-remove-node x z)
	(endif))

(defun lh-remove-tail (x y z)
	;inputs
	;x = list head
	;y = temp
	;z = temp
	;outputs
	;x = list tailpred
	;y = 0
	;z = temp
	;else
	;x = list node pred
	;y = list node
	;z = list node succ

	(lh-get-tail x x)
	(ln-get-pred x y)
	(vpif '(y != 0))
		(vp-cpy-rr x y)
		(ln-remove-node x z)
	(endif))

(defun lh-is-empty (x y)
	;inputs
	;x = list head
	;y = temp
	;outputs
	;x = list head
	;y = 0 if empty, else not

	(ln-get-succ x y)
	(ln-get-succ y y))

;;;;;;;;;;;;;;;;;;
; list loop macros
;;;;;;;;;;;;;;;;;;

(defun loop-list-forward (x y z)
	;inputs
	;x = list head
	;y = list node
	;z = list node look ahead
	;outputs
	;x = list head
	;y = list node
	;z = list node look ahead

	(lh-get-head x z)
	(loop-start)
		(vp-cpy-rr z y)
		(ln-get-succ z z)
		(breakif '(z == 0)))

(defun loop-list-backward (x y z)
	;inputs
	;x = list head
	;y = list node
	;z = list node look ahead
	;outputs
	;x = list head
	;y = list node
	;z = list node look ahead

	(lh-get-tail x z)
	(loop-start)
		(vp-cpy-rr z y)
		(ln-get-pred z z)
		(breakif '(z == 0)))

(defun loop-flist-forward (x y z)
	;inputs
	;x = list head
	;y = list node
	;z = list last
	;outputs
	;x = list head
	;y = list node
	;z = list last

	(vp-cpy-rr x y)
	(loop-start)
		(ln-next-fnode y z))

;;;;;;;;;;;;;;;;;;;;;
; forward list macros
;;;;;;;;;;;;;;;;;;;;;

(defun ln-next-fnode (x y)
	;inputs
	;x = node
	;y = last
	;trashes
	;x, y

	(vp-cpy-rr x y)
	(vp-cpy-ir x ln_fnode_next x)
	(breakif '(x == 0)))

(defun ln-remove-fnode (x y)
	;inputs
	;x = node
	;y = last
	;trashes
	;x

	(vp-cpy-ir x ln_fnode_next x)
	(vp-cpy-ri x y ln_fnode_next)
	(vp-cpy-rr y x))

(defun ln-add-fnode (x y z)
	;inputs
	;x = list
	;y = node
	;z = temp
	;trashes
	;z

	(vp-cpy-ir x ln_fnode_next z)
	(vp-cpy-ri z y ln_fnode_next)
	(vp-cpy-ri y x ln_fnode_next))
