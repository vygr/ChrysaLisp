;;;;;;;;;;;;;;;;;;;;;;;;
; expression evaluator
;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun expr-eval (expr)
	; (expr-eval expr) -> result
	;evaluate an expression
	(cond
		((num? expr) expr)
		((sym? expr)
			;lookup variable or constant
			(cond
				((eql expr 'pi) 3.14159265358979)
				((eql expr 'e) 2.71828182845905)
				((eql expr 'phi) 1.61803398874989)
				(:t (throw "Unknown symbol" expr))))
		((list? expr)
			(if (= (length expr) 0)
				:nil
				(progn
					(defq op (first expr) args (rest expr))
					(expr-eval-op op (map (const expr-eval) args)))))
		(:t (throw "Invalid expression" expr))))

(defun expr-eval-op (op args)
	; (expr-eval-op op args) -> result
	;evaluate an operator with its arguments
	(cond
		;arithmetic operators
		((eql op '+)
			(if (= (length args) 0)
				0
				(apply (const +) args)))
		((eql op '-)
			(cond
				((= (length args) 0) 0)
				((= (length args) 1) (- 0 (first args)))
				(:t (apply (const -) args))))
		((eql op '*)
			(if (= (length args) 0)
				1
				(apply (const *) args)))
		((eql op '/)
			(cond
				((= (length args) 0) (throw "Division needs arguments"))
				((= (length args) 1) (/ 1 (first args)))
				(:t (apply (const /) args))))
		((eql op '%)
			(if (= (length args) 2)
				(% (first args) (second args))
				(throw "Modulo needs 2 arguments")))
		((eql op '^)
			(if (= (length args) 2)
				(** (first args) (second args))
				(throw "Power needs 2 arguments")))
		;comparison operators
		((eql op '=)
			(if (apply (const =) args) 1 0))
		((eql op '<)
			(if (apply (const <) args) 1 0))
		((eql op '>)
			(if (apply (const >) args) 1 0))
		((eql op '<=)
			(if (apply (const <=) args) 1 0))
		((eql op '>=)
			(if (apply (const >=) args) 1 0))
		((eql op '!=)
			(if (apply (const /=) args) 1 0))
		;logical operators
		((eql op 'and)
			(if (every (# (/= %0 0)) args) 1 0))
		((eql op 'or)
			(if (some (# (/= %0 0)) args) 1 0))
		((eql op 'not)
			(if (= (first args) 0) 1 0))
		;mathematical functions
		((eql op 'abs)
			(if (= (length args) 1)
				(abs (first args))
				(throw "abs needs 1 argument")))
		((eql op 'sqrt)
			(if (= (length args) 1)
				(i2f (sqrt (f2i (first args))))
				(throw "sqrt needs 1 argument")))
		((eql op 'min)
			(apply (const min) args))
		((eql op 'max)
			(apply (const max) args))
		((eql op 'floor)
			(if (= (length args) 1)
				(>> (<< (first args) 32) 32)
				(throw "floor needs 1 argument")))
		((eql op 'ceil)
			(if (= (length args) 1)
				(defq x (first args))
				(if (= x (>> (<< x 32) 32))
					x
					(inc (>> (<< x 32) 32)))
				(throw "ceil needs 1 argument")))
		;special functions
		((eql op 'factorial)
			(if (= (length args) 1)
				(expr-factorial (first args))
				(throw "factorial needs 1 argument")))
		((eql op 'fib)
			(if (= (length args) 1)
				(expr-fibonacci (first args))
				(throw "fib needs 1 argument")))
		((eql op 'gcd)
			(if (= (length args) 2)
				(expr-gcd (first args) (second args))
				(throw "gcd needs 2 arguments")))
		((eql op 'lcm)
			(if (= (length args) 2)
				(expr-lcm (first args) (second args))
				(throw "lcm needs 2 arguments")))
		;list operations
		((eql op 'sum)
			(apply (const +) args))
		((eql op 'product)
			(apply (const *) args))
		((eql op 'mean)
			(if (> (length args) 0)
				(/ (apply (const +) args) (length args))
				0))
		(:t (throw "Unknown operator" op))))

(defun expr-factorial (n)
	; (expr-factorial n) -> n!
	;calculate factorial
	(if (<= n 1)
		1
		(* n (expr-factorial (dec n)))))

(defun expr-fibonacci (n)
	; (expr-fibonacci n) -> fib(n)
	;calculate fibonacci number
	(cond
		((<= n 0) 0)
		((= n 1) 1)
		(:t (+ (expr-fibonacci (- n 1)) (expr-fibonacci (- n 2))))))

(defun expr-gcd (a b)
	; (expr-gcd a b) -> gcd(a,b)
	;calculate greatest common divisor
	(if (= b 0)
		a
		(expr-gcd b (% a b))))

(defun expr-lcm (a b)
	; (expr-lcm a b) -> lcm(a,b)
	;calculate least common multiple
	(/ (* a b) (expr-gcd a b)))

;export module functions
(export (env-peek)
	'(expr-eval expr-eval-op expr-factorial expr-fibonacci
		expr-gcd expr-lcm))
(env-pop)
