(import "lib/options/options.inc")
(import "lib/expr/parser.inc")
(import "lib/expr/serializer.inc")
(import "lib/expr/eval.inc")
(import "lib/expr/stats.inc")

(defq usage `(
(("-h" "--help")
"Usage: expr [options] [expression] ...

	options:
		-h --help: this help info.
		-i --input format: input format (sexp, infix, prefix, json). default: auto
		-o --output format: output format (sexp, pretty, json, xml, dot, infix, prefix, ast, tree, rainbow). default: sexp
		-e --eval: evaluate the expression and show result.
		-s --stats: show expression statistics.
		-m --multi: process multiple formats in parallel.
		-v --verbose: verbose output.
		-c --color: enable colored output.

	Input Formats:
		sexp:   S-expressions (e.g., '(+ 1 (* 2 3))')
		infix:  Infix notation (e.g., '1 + 2 * 3')
		prefix: Prefix notation (e.g., '+ 1 * 2 3')
		json:   JSON-like (e.g., '{\"op\":\"+\",\"args\":[1,2]}')
		auto:   Automatically detect format

	Output Formats:
		sexp:    Canonical S-expression
		pretty:  Pretty-printed S-expression with indentation
		json:    JSON representation
		xml:     XML representation
		dot:     GraphViz DOT format for visualization
		infix:   Infix notation with proper precedence
		prefix:  Prefix notation
		ast:     ASCII art abstract syntax tree
		tree:    Hierarchical tree view
		rainbow: Rainbow-colored pretty print

	Examples:
		expr '(+ 1 (* 2 3))' --eval
		expr '1 + 2 * 3' -i infix -o sexp
		expr '(factorial 5)' -e -s
		expr '(+ 1 2 3 4)' -o dot | dot -Tpng > expr.png
		echo '(* (+ 1 2) (- 5 3))' | expr -o ast

	If no expression given on command line, reads from stdin.")
(("-i" "--input") ,(opt-word 'opt_input))
(("-o" "--output") ,(opt-word 'opt_output))
(("-e" "--eval") ,(opt-flag 'opt_eval))
(("-s" "--stats") ,(opt-flag 'opt_stats))
(("-m" "--multi") ,(opt-flag 'opt_multi))
(("-v" "--verbose") ,(opt-flag 'opt_verbose))
(("-c" "--color") ,(opt-flag 'opt_color))
))

(defun process-expression (expr)
	;parse, serialize, eval and show stats for an expression
	(catch (progn
		(when opt_verbose
			(print "Input: " expr))
		;parse the expression
		(defq parsed (expr-parse expr opt_input))
		(when opt_verbose
			(print "Parsed: " parsed))
		;serialize the expression
		(defq serialized (expr-serialize parsed opt_output opt_color))
		(print serialized)
		;evaluate if requested
		(when opt_eval
			(defq result (expr-eval parsed))
			(print "Result: " result))
		;show stats if requested
		(when opt_stats
			(expr-show-stats parsed))
		:t)
		(progn
			(print "Error: " _)
			:nil)))

(defun process-multi-format (expr)
	;show the expression in multiple formats side-by-side
	(catch (progn
		(defq parsed (expr-parse expr opt_input))
		(print "")
		(print "===== Expression Analysis =====")
		(print "")
		(print "Input Format: " opt_input)
		(print "Original: " expr)
		(print "")
		(print "--- S-Expression ---")
		(print (expr-serialize parsed 'sexp :nil))
		(print "")
		(print "--- Pretty Print ---")
		(print (expr-serialize parsed 'pretty :nil))
		(print "")
		(print "--- Infix Notation ---")
		(print (expr-serialize parsed 'infix :nil))
		(print "")
		(print "--- Prefix Notation ---")
		(print (expr-serialize parsed 'prefix :nil))
		(print "")
		(print "--- JSON ---")
		(print (expr-serialize parsed 'json :nil))
		(print "")
		(print "--- XML ---")
		(print (expr-serialize parsed 'xml :nil))
		(print "")
		(print "--- AST Tree ---")
		(print (expr-serialize parsed 'ast :nil))
		(when opt_eval
			(print "")
			(print "--- Evaluation ---")
			(defq result (expr-eval parsed))
			(print "Result: " result))
		(when opt_stats
			(print "")
			(print "--- Statistics ---")
			(expr-show-stats parsed))
		(print "")
		(print "==============================")
		:t)
		(progn
			(print "Error: " _)
			:nil)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_input 'auto opt_output 'sexp
				opt_eval :nil opt_stats :nil opt_multi :nil
				opt_verbose :nil opt_color :nil
				args (options stdio usage)))
		;process expressions
		(if (<= (length args) 1)
			;read from stdin
			(lines! (lambda (line)
				(unless (eql line "")
					(if opt_multi
						(process-multi-format line)
						(process-expression line))))
				(io-stream 'stdin))
			;process from args
			(each (lambda (expr)
				(if opt_multi
					(process-multi-format expr)
					(process-expression expr)))
				(rest args)))))
