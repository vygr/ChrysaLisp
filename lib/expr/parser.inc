;;;;;;;;;;;;;;;;;;;;
; expression parser
;;;;;;;;;;;;;;;;;;;;

(import "lib/text/charclass.inc")

;module
(env-push)

(defun expr-parse (expr fmt)
	; (expr-parse expr format) -> parsed-expr
	;parse an expression from a string in the given format
	(cond
		((eql fmt 'auto)
			(expr-parse-auto expr))
		((eql fmt 'sexp)
			(expr-parse-sexp expr))
		((eql fmt 'infix)
			(expr-parse-infix expr))
		((eql fmt 'prefix)
			(expr-parse-prefix expr))
		((eql fmt 'json)
			(expr-parse-json expr))
		(:t (throw "Unknown input format" fmt))))

(defun expr-parse-auto (expr)
	; (expr-parse-auto expr) -> parsed-expr
	;automatically detect format and parse
	(defq trimmed (trim expr))
	(cond
		((or (eql (first trimmed) "(") (eql (first trimmed) "'"))
			(expr-parse-sexp trimmed))
		((eql (first trimmed) "{")
			(expr-parse-json trimmed))
		((find "+*-/%" trimmed)
			(expr-parse-infix trimmed))
		(:t (expr-parse-prefix trimmed))))

(defun expr-parse-sexp (expr)
	; (expr-parse-sexp expr) -> parsed-expr
	;parse an S-expression
	(defq trimmed (trim expr))
	(if (eql (first trimmed) "'")
		(setq trimmed (slice 1 -1 trimmed)))
	(read (string-stream trimmed)))

(defun expr-tokenize (expr)
	; (expr-tokenize expr) -> (token ...)
	;tokenize an infix or prefix expression
	(defq tokens (list) i 0 len (length expr) buf "")
	(defun flush-buf ()
		(unless (eql buf "")
			(push tokens
				(if (every (const bfind) buf +char_class_digit)
					(str-to-num buf)
					(sym buf)))
			(setq buf "")))
	(while (< i len)
		(defq ch (elem-get expr i))
		(cond
			((bfind ch +char_class_white_space)
				(flush-buf))
			((find "+-*/%^()[]{}," ch)
				(flush-buf)
				(push tokens (sym ch)))
			(:t (setq buf (cat buf ch))))
		(setq i (inc i)))
	(flush-buf)
	tokens)

(defun expr-parse-infix (expr)
	; (expr-parse-infix expr) -> parsed-expr
	;parse infix notation using shunting yard algorithm
	(defq tokens (expr-tokenize expr)
		output (list) operators (list))
	(defun precedence (op)
		(cond
			((some (const eql) (list '+ '-) op) 1)
			((some (const eql) (list '* '/ '%) op) 2)
			((eql op '^) 3)
			(:t 0)))
	(defun right-assoc? (op)
		(eql op '^))
	(each (lambda (token)
		(cond
			((num? token)
				(push output token))
			((or (eql token '|(|) (eql token '|[|))
				(push operators token))
			((or (eql token '|)|) (eql token '|]|))
				(while (and (> (length operators) 0)
						(not (eql (last operators) '|(|))
						(not (eql (last operators) '|[|)))
					(defq op (pop operators))
					(defq b (pop output) a (pop output))
					(push output (list op a b)))
				(unless (= (length operators) 0)
					(pop operators)))
			((sym? token)
				(defq prec (precedence token))
				(when (> prec 0)
					(while (and (> (length operators) 0)
							(defq top (last operators))
							(not (eql top '|(|))
							(not (eql top '|[|))
							(defq top-prec (precedence top))
							(or (> top-prec prec)
								(and (= top-prec prec)
									(not (right-assoc? token)))))
						(defq op (pop operators))
						(defq b (pop output) a (pop output))
						(push output (list op a b))))
				(push operators token))))
		tokens)
	(while (> (length operators) 0)
		(defq op (pop operators))
		(defq b (pop output) a (pop output))
		(push output (list op a b)))
	(if (= (length output) 1)
		(first output)
		(throw "Invalid infix expression" expr)))

(defun expr-parse-prefix (expr)
	; (expr-parse-prefix expr) -> parsed-expr
	;parse prefix notation
	(defq tokens (expr-tokenize expr))
	(defun parse-tokens (toks)
		(if (= (length toks) 0)
			(throw "Unexpected end of expression"))
		(defq token (pop toks 0))
		(cond
			((num? token) token)
			((sym? token)
				(defq args (list))
				(while (and (> (length toks) 0)
						(not (find ")" (str (first toks)))))
					(push args (parse-tokens toks)))
				(if (= (length args) 0)
					token
					(list token (splice args))))))
	(parse-tokens (list (splice tokens))))

(defun expr-parse-json (expr)
	; (expr-parse-json expr) -> parsed-expr
	;parse JSON-like expression format
	;simplified JSON parser for expressions
	(defq trimmed (trim expr))
	(cond
		((or (eql (first trimmed) "{") (eql (first trimmed) "["))
			;TODO: implement full JSON parsing
			(throw "JSON parsing not yet fully implemented" expr))
		(:t (expr-parse-sexp trimmed))))

;export module functions
(export (env-peek)
	'(expr-parse expr-parse-auto expr-parse-sexp expr-parse-infix
		expr-parse-prefix expr-parse-json expr-tokenize))
(env-pop)
