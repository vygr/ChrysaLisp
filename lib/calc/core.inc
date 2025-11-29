;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Core Calculator Functions
; Pure computation, no GUI dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic Arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calc-add (a b)
	; (calc-add a b) -> a + b
	(+ a b))

(defun calc-sub (a b)
	; (calc-sub a b) -> a - b
	(- a b))

(defun calc-mul (a b)
	; (calc-mul a b) -> a * b
	(* a b))

(defun calc-div (a b)
	; (calc-div a b) -> a / b
	; Returns :error if divide by zero
	(if (= b 0)
		:error
		(/ a b)))

(defun calc-mod (a b)
	; (calc-mod a b) -> a % b
	; Returns :error if divide by zero
	(if (= b 0)
		:error
		(% a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bitwise Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calc-and (a b)
	; (calc-and a b) -> a AND b
	(logand a b))

(defun calc-or (a b)
	; (calc-or a b) -> a OR b
	(logior a b))

(defun calc-xor (a b)
	; (calc-xor a b) -> a XOR b
	(logxor a b))

(defun calc-not (a)
	; (calc-not a) -> NOT a
	(lognot a))

(defun calc-shl (a bits)
	; (calc-shl a bits) -> a << bits
	(<< a bits))

(defun calc-shr (a bits)
	; (calc-shr a bits) -> a >> bits (logical)
	(>> a bits))

(defun calc-asr (a bits)
	; (calc-asr a bits) -> a >> bits (arithmetic)
	(>>> a bits))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Base Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +digit_chars "0123456789ABCDEF")

(defun calc-to-base (num base)
	; (calc-to-base num base) -> string
	; Convert number to string in given base (2, 8, 10, or 16)
	(cond
		((= num 0) "0")
		(:t (progn
			(defq result "" is_neg :nil)
			(when (< num 0)
				(setq is_neg :t num (abs num)))
			(while (> num 0)
				(defq digit (% num base))
				(setq result (cat (slice +digit_chars digit (inc digit)) result)
					  num (/ num base)))
			(if is_neg (cat "-" result) result)))))

(defun calc-from-base (str base)
	; (calc-from-base str base) -> number
	; Convert string in given base to number
	; Returns :error if invalid
	(defq num 0 is_neg :nil)
	(when (starts-with "-" str)
		(setq is_neg :t str (slice str 1 -1)))
	(defq valid :t)
	(each (lambda (c)
		(defq idx (find c +digit_chars))
		(cond
			((and idx (< idx base))
				(setq num (+ (* num base) idx)))
			(:t (setq valid :nil))))
		str)
	(if valid
		(if is_neg (neg num) num)
		:error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Expression Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calc-eval-binary (op a b)
	; (calc-eval-binary op a b) -> result
	; Evaluate binary operation
	(case op
		("+" (calc-add a b))
		("-" (calc-sub a b))
		("*" (calc-mul a b))
		("/" (calc-div a b))
		("%" (calc-mod a b))
		("AND" (calc-and a b))
		("OR" (calc-or a b))
		("XOR" (calc-xor a b))
		("<<" (calc-shl a b))
		(">>" (calc-shr a b))
		(">>>" (calc-asr a b))
		(:t :error)))

(defun calc-eval-unary (op a)
	; (calc-eval-unary op a) -> result
	; Evaluate unary operation
	(case op
		("NEG" (neg a))
		("NOT" (calc-not a))
		(:t :error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple Expression Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calc-parse-number (str base)
	; (calc-parse-number str base) -> number
	; Parse a number string in the given base
	(calc-from-base str base))

(defun calc-simple-eval (num1 op num2)
	; (calc-simple-eval num1 op num2) -> result
	; Evaluate simple binary expression
	; num1 and num2 are already numbers
	; op is a string like "+", "-", etc.
	(calc-eval-binary op num1 num2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Stack-based Calculator
; For RPN (Reverse Polish Notation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calc-rpn-eval (tokens base)
	; (calc-rpn-eval tokens base) -> result
	; Evaluate RPN expression
	; tokens is a list of strings/numbers
	; Returns final result or :error
	(defq stack (list))
	(each (lambda (token)
		(cond
			; Check if it's an operator
			((find token '("+" "-" "*" "/" "%" "AND" "OR" "XOR" "<<" ">>" ">>>"))
				(if (>= (length stack) 2)
					(progn
						(defq b (pop stack) a (pop stack))
						(defq result (calc-eval-binary token a b))
						(if (eql result :error)
							(setq stack '(:error))
							(push stack result)))
					(setq stack '(:error))))
			; Check if it's a unary operator
			((find token '("NEG" "NOT"))
				(if (>= (length stack) 1)
					(progn
						(defq a (pop stack))
						(defq result (calc-eval-unary token a))
						(if (eql result :error)
							(setq stack '(:error))
							(push stack result)))
					(setq stack '(:error))))
			; Otherwise, it's a number
			(:t (progn
				(defq num (if (num? token)
					token
					(calc-parse-number token base)))
				(if (eql num :error)
					(setq stack '(:error))
					(push stack num))))))
		tokens)
	(if (and (= (length stack) 1) (not (eql (first stack) :error)))
		(first stack)
		:error))

;module
(export-symbols '(calc-add calc-sub calc-mul calc-div calc-mod
	calc-and calc-or calc-xor calc-not
	calc-shl calc-shr calc-asr
	calc-to-base calc-from-base
	calc-eval-binary calc-eval-unary
	calc-simple-eval calc-rpn-eval
	calc-parse-number))
(env-pop)
