;;;;;;;;;;;;;;;;;;;;;;;;
; expression statistics
;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/collections.inc")

;module
(env-push)

(defun expr-show-stats (expr)
	; (expr-show-stats expr)
	;display statistics about an expression
	(defq stats (expr-stats expr))
	(print "Statistics:")
	(print "  Total nodes: " (. stats :find :total-nodes))
	(print "  Depth: " (. stats :find :depth))
	(print "  Operators: " (. stats :find :operators))
	(print "  Operands: " (. stats :find :operands))
	(print "  Numbers: " (. stats :find :numbers))
	(print "  Symbols: " (. stats :find :symbols))
	(print "  Lists: " (. stats :find :lists))
	(defq op-freq (. stats :find :op-frequency))
	(when (> (length op-freq) 0)
		(print "  Operator frequency:")
		(. op-freq :each (lambda (op count)
			(print "    " op ": " count)))))

(defun expr-stats (expr)
	; (expr-stats expr) -> stats-map
	;collect statistics about an expression
	(defq stats (scatter (Fmap)
		:total-nodes 0
		:depth 0
		:operators 0
		:operands 0
		:numbers 0
		:symbols 0
		:lists 0
		:op-frequency (Fmap)))
	(expr-stats-collect expr stats 0)
	stats)

(defun expr-stats-collect (expr stats depth)
	; (expr-stats-collect expr stats depth)
	;recursively collect statistics
	(. stats :update :total-nodes (inc (. stats :find :total-nodes)))
	(. stats :update :depth (max (. stats :find :depth) depth))
	(cond
		((num? expr)
			(. stats :update :operands (inc (. stats :find :operands)))
			(. stats :update :numbers (inc (. stats :find :numbers))))
		((sym? expr)
			(. stats :update :operands (inc (. stats :find :operands)))
			(. stats :update :symbols (inc (. stats :find :symbols))))
		((list? expr)
			(. stats :update :lists (inc (. stats :find :lists)))
			(unless (= (length expr) 0)
				(defq op (first expr))
				(. stats :update :operators (inc (. stats :find :operators)))
				(defq op-freq (. stats :find :op-frequency))
				(defq op-str (str op))
				(. op-freq :update op-str (inc (if (. op-freq :find op-str)
					(. op-freq :find op-str) 0)))
				(each (lambda (arg)
					(expr-stats-collect arg stats (inc depth)))
					(rest expr))))))

(defun expr-complexity (expr)
	; (expr-complexity expr) -> complexity-score
	;calculate a complexity score for an expression
	(defq stats (expr-stats expr))
	(+ (* 1 (. stats :find :total-nodes))
		(* 2 (. stats :find :depth))
		(* 3 (. stats :find :operators))))

(defun expr-depth (expr)
	; (expr-depth expr) -> depth
	;calculate the depth of an expression tree
	(cond
		((num? expr) 1)
		((sym? expr) 1)
		((list? expr)
			(if (= (length expr) 0)
				1
				(inc (apply (const max)
					(map (const expr-depth) (rest expr))))))
		(:t 1)))

(defun expr-node-count (expr)
	; (expr-node-count expr) -> count
	;count total nodes in expression tree
	(cond
		((num? expr) 1)
		((sym? expr) 1)
		((list? expr)
			(if (= (length expr) 0)
				1
				(apply (const +) 1
					(map (const expr-node-count) expr))))
		(:t 1)))

(defun expr-operator-count (expr)
	; (expr-operator-count expr) -> count
	;count operators in expression
	(cond
		((num? expr) 0)
		((sym? expr) 0)
		((list? expr)
			(if (= (length expr) 0)
				0
				(apply (const +) 1
					(map (const expr-operator-count) (rest expr)))))
		(:t 0)))

(defun expr-operand-count (expr)
	; (expr-operand-count expr) -> count
	;count operands (leaf nodes) in expression
	(cond
		((num? expr) 1)
		((sym? expr) 1)
		((list? expr)
			(if (= (length expr) 0)
				0
				(apply (const +)
					(map (const expr-operand-count) (rest expr)))))
		(:t 1)))

;export module functions
(export (env-peek)
	'(expr-show-stats expr-stats expr-stats-collect expr-complexity
		expr-depth expr-node-count expr-operator-count expr-operand-count))
(env-pop)
