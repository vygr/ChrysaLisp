;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; symbolic differentiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun diff (expr var)
	; (diff expr var) -> derivative
	;compute the derivative of expr with respect to var
	(cond
		;constant rule: d/dx c = 0
		((num? expr) 0)
		;variable rule: d/dx x = 1, d/dx y = 0
		((sym? expr)
			(if (eql expr var) 1 0))
		;list expression
		((list? expr)
			(if (= (length expr) 0)
				0
				(progn
					(defq op (first expr) args (rest expr))
					(diff-op op args var))))
		;default
		(:t 0)))

(defun diff-op (op args var)
	; (diff-op op args var) -> derivative
	;differentiate an operation
	(cond
		;sum rule: d/dx (f + g + ...) = df/dx + dg/dx + ...
		((eql op '+)
			(if (= (length args) 0)
				0
				(list '+ (splice (map (# (diff %0 var)) args)))))

		;difference rule: d/dx (f - g) = df/dx - dg/dx
		((eql op '-)
			(cond
				((= (length args) 0) 0)
				((= (length args) 1)
					(list '- (diff (first args) var)))
				(:t (list '- (diff (first args) var) (diff (second args) var)))))

		;product rule: d/dx (f * g) = f * dg/dx + g * df/dx
		((eql op '*)
			(cond
				((= (length args) 0) 0)
				((= (length args) 1) (diff (first args) var))
				((= (length args) 2)
					(defq f (first args) g (second args))
					(list '+
						(list '* f (diff g var))
						(list '* g (diff f var))))
				(:t ;multiple arguments - use product rule recursively
					(diff (list '* (first args)
						(list '* (splice (rest args)))) var))))

		;quotient rule: d/dx (f / g) = (g * df/dx - f * dg/dx) / g^2
		((eql op '/)
			(if (= (length args) 2)
				(defq f (first args) g (second args))
				(list '/
					(list '-
						(list '* g (diff f var))
						(list '* f (diff g var)))
					(list '^ g 2))
				0))

		;power rule: d/dx (f ^ n) = n * f^(n-1) * df/dx (chain rule)
		((eql op '^)
			(if (= (length args) 2)
				(defq f (first args) n (second args))
				(cond
					;if exponent is constant: d/dx x^n = n*x^(n-1)
					((num? n)
						(list '* n (list '* (list '^ f (- n 1)) (diff f var))))
					;if base is constant: d/dx c^f = c^f * ln(c) * df/dx
					((num? f)
						(list '* (list '^ f n)
							(list '* (list 'ln f) (diff n var))))
					;general case: d/dx f^g = f^g * (g*df/dx/f + ln(f)*dg/dx)
					(:t
						(list '* (list '^ f n)
							(list '+
								(list '/ (list '* n (diff f var)) f)
								(list '* (list 'ln f) (diff n var))))))
				0))

		;trigonometric functions
		((eql op 'sin)
			(if (= (length args) 1)
				(defq f (first args))
				(list '* (list 'cos f) (diff f var))
				0))

		((eql op 'cos)
			(if (= (length args) 1)
				(defq f (first args))
				(list '* (list '- 0 (list 'sin f)) (diff f var))
				0))

		((eql op 'tan)
			(if (= (length args) 1)
				(defq f (first args))
				(list '* (list '^ (list 'sec f) 2) (diff f var))
				0))

		;exponential and logarithm
		((eql op 'exp)
			(if (= (length args) 1)
				(defq f (first args))
				(list '* (list 'exp f) (diff f var))
				0))

		((eql op 'ln)
			(if (= (length args) 1)
				(defq f (first args))
				(list '/ (diff f var) f)
				0))

		((eql op 'log)
			(if (= (length args) 1)
				(defq f (first args))
				(list '/ (diff f var) (list '* f (list 'ln 10)))
				0))

		;square root: d/dx sqrt(f) = 1/(2*sqrt(f)) * df/dx
		((eql op 'sqrt)
			(if (= (length args) 1)
				(defq f (first args))
				(list '/ (diff f var) (list '* 2 (list 'sqrt f)))
				0))

		;absolute value: d/dx |f| = f/|f| * df/dx (sign function)
		((eql op 'abs)
			(if (= (length args) 1)
				(defq f (first args))
				(list '* (list '/ f (list 'abs f)) (diff f var))
				0))

		;min/max - not differentiable at boundary, but we can try
		((eql op 'min)
			;treat as first argument for simplicity
			(if (> (length args) 0)
				(diff (first args) var)
				0))

		((eql op 'max)
			;treat as first argument for simplicity
			(if (> (length args) 0)
				(diff (first args) var)
				0))

		;unknown operator - return symbolic derivative
		(:t (list 'd/dx (list op (splice args))))))

(defun partial-diff (expr vars)
	; (partial-diff expr (var ...)) -> (derivative ...)
	;compute partial derivatives with respect to multiple variables
	(map (# (diff expr %0)) vars))

(defun gradient (expr vars)
	; (gradient expr (var ...)) -> (grad ...)
	;compute the gradient vector
	(list 'gradient (splice (partial-diff expr vars))))

(defun nth-derivative (expr var n)
	; (nth-derivative expr var n) -> nth-derivative
	;compute the nth derivative
	(if (<= n 0)
		expr
		(nth-derivative (diff expr var) var (dec n))))

;export module functions
(export (env-peek)
	'(diff diff-op partial-diff gradient nth-derivative))
(env-pop)
