;;;;;;;;;;;;;;;;;;;;;;;;;;;
; expression simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun simplify (expr)
	; (simplify expr) -> simplified-expr
	;simplify an expression by applying algebraic rules
	(defq result (simplify-once expr))
	;keep simplifying until no more changes
	(if (eql result expr)
		result
		(simplify result)))

(defun simplify-once (expr)
	; (simplify-once expr) -> expr
	;apply one round of simplification rules
	(cond
		((num? expr) expr)
		((sym? expr) expr)
		((list? expr)
			(if (= (length expr) 0)
				expr
				(progn
					(defq op (first expr) args (rest expr))
					;first simplify all arguments
					(defq simple-args (map (const simplify-once) args))
					;then simplify the operation
					(simplify-op op simple-args))))
		(:t expr)))

(defun simplify-op (op args)
	; (simplify-op op args) -> expr
	;simplify an operation with simplified arguments
	(cond
		;addition simplifications
		((eql op '+)
			(cond
				;empty sum
				((= (length args) 0) 0)
				;single argument
				((= (length args) 1) (first args))
				;constant folding
				((every (const num?) args)
					(apply (const +) args))
				;remove zeros and flatten
				(:t
					(defq non-zero (filter (# (not (eql %0 0))) args))
					(cond
						((= (length non-zero) 0) 0)
						((= (length non-zero) 1) (first non-zero))
						(:t (list '+ (splice non-zero)))))))

		;subtraction simplifications
		((eql op '-)
			(cond
				((= (length args) 0) 0)
				((= (length args) 1)
					(if (num? (first args))
						(- 0 (first args))
						(if (eql (first args) 0)
							0
							(list '- (first args)))))
				((= (length args) 2)
					(defq a (first args) b (second args))
					(cond
						;constant folding
						((and (num? a) (num? b)) (- a b))
						;x - 0 = x
						((eql b 0) a)
						;0 - x = -x
						((eql a 0) (list '- b))
						;x - x = 0
						((eql a b) 0)
						(:t (list '- a b))))
				(:t (list '- (splice args)))))

		;multiplication simplifications
		((eql op '*)
			(cond
				((= (length args) 0) 1)
				((= (length args) 1) (first args))
				;constant folding
				((every (const num?) args)
					(apply (const *) args))
				;check for zeros
				((some (# (eql %0 0)) args) 0)
				;remove ones and flatten
				(:t
					(defq non-one (filter (# (not (eql %0 1))) args))
					(cond
						((= (length non-one) 0) 1)
						((= (length non-one) 1) (first non-one))
						(:t (list '* (splice non-one)))))))

		;division simplifications
		((eql op '/)
			(if (= (length args) 2)
				(defq a (first args) b (second args))
				(cond
					;constant folding
					((and (num? a) (num? b)) (/ a b))
					;0 / x = 0
					((eql a 0) 0)
					;x / 1 = x
					((eql b 1) a)
					;x / x = 1
					((eql a b) 1)
					(:t (list '/ a b)))
				(list '/ (splice args))))

		;power simplifications
		((eql op '^)
			(if (= (length args) 2)
				(defq base (first args) exp (second args))
				(cond
					;constant folding
					((and (num? base) (num? exp))
						(** base exp))
					;x ^ 0 = 1
					((eql exp 0) 1)
					;x ^ 1 = x
					((eql exp 1) base)
					;0 ^ x = 0 (assuming x > 0)
					((eql base 0) 0)
					;1 ^ x = 1
					((eql base 1) 1)
					(:t (list '^ base exp)))
				(list '^ (splice args))))

		;logarithm simplifications
		((eql op 'ln)
			(if (= (length args) 1)
				(defq arg (first args))
				(cond
					;ln(1) = 0
					((eql arg 1) 0)
					;ln(e) = 1
					((eql arg 'e) 1)
					;ln(exp(x)) = x
					((and (list? arg) (eql (first arg) 'exp))
						(second arg))
					(:t (list 'ln arg)))
				(list 'ln (splice args))))

		;exponential simplifications
		((eql op 'exp)
			(if (= (length args) 1)
				(defq arg (first args))
				(cond
					;exp(0) = 1
					((eql arg 0) 1)
					;exp(ln(x)) = x
					((and (list? arg) (eql (first arg) 'ln))
						(second arg))
					(:t (list 'exp arg)))
				(list 'exp (splice args))))

		;square root simplifications
		((eql op 'sqrt)
			(if (= (length args) 1)
				(defq arg (first args))
				(cond
					;sqrt(0) = 0
					((eql arg 0) 0)
					;sqrt(1) = 1
					((eql arg 1) 1)
					;sqrt(x^2) = |x| (we'll simplify to x for now)
					((and (list? arg) (eql (first arg) '^)
						(eql (third arg) 2))
						(second arg))
					(:t (list 'sqrt arg)))
				(list 'sqrt (splice args))))

		;absolute value simplifications
		((eql op 'abs)
			(if (= (length args) 1)
				(defq arg (first args))
				(cond
					;abs(0) = 0
					((eql arg 0) 0)
					;abs(positive number)
					((and (num? arg) (> arg 0)) arg)
					;abs(negative number)
					((and (num? arg) (< arg 0)) (- 0 arg))
					;abs(abs(x)) = abs(x)
					((and (list? arg) (eql (first arg) 'abs))
						arg)
					(:t (list 'abs arg)))
				(list 'abs (splice args))))

		;trigonometric simplifications
		((eql op 'sin)
			(if (= (length args) 1)
				(defq arg (first args))
				(cond
					;sin(0) = 0
					((eql arg 0) 0)
					(:t (list 'sin arg)))
				(list 'sin (splice args))))

		((eql op 'cos)
			(if (= (length args) 1)
				(defq arg (first args))
				(cond
					;cos(0) = 1
					((eql arg 0) 1)
					(:t (list 'cos arg)))
				(list 'cos (splice args))))

		;default - no simplification
		(:t (list op (splice args)))))

(defun expand (expr)
	; (expand expr) -> expanded-expr
	;expand products and powers
	(cond
		((num? expr) expr)
		((sym? expr) expr)
		((list? expr)
			(if (= (length expr) 0)
				expr
				(progn
					(defq op (first expr) args (rest expr))
					(expand-op op (map (const expand) args)))))
		(:t expr)))

(defun expand-op (op args)
	; (expand-op op args) -> expr
	;expand an operation
	(cond
		;(a + b) * c = a*c + b*c
		((eql op '*)
			(if (= (length args) 2)
				(defq a (first args) b (second args))
				(cond
					;distribute over addition
					((and (list? a) (eql (first a) '+))
						(list '+ (splice (map (# (list '* %0 b)) (rest a)))))
					((and (list? b) (eql (first b) '+))
						(list '+ (splice (map (# (list '* a %0)) (rest b)))))
					(:t (list '* a b)))
				(list '* (splice args))))
		;default
		(:t (list op (splice args)))))

(defun collect-like-terms (expr)
	; (collect-like-terms expr) -> expr
	;collect like terms in a sum
	;TODO: implement coefficient collection
	expr)

;export module functions
(export (env-peek)
	'(simplify simplify-once simplify-op expand expand-op collect-like-terms))
(env-pop)
