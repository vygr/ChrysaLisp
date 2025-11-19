;;;;;;;;;;;;;;;;;;;;;;;;;;
; expression serializer
;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun expr-serialize (expr fmt color)
	; (expr-serialize expr format color) -> string
	;serialize an expression to a string in the given format
	(cond
		((eql fmt 'sexp)
			(expr-serialize-sexp expr))
		((eql fmt 'pretty)
			(expr-serialize-pretty expr 0 color))
		((eql fmt 'json)
			(expr-serialize-json expr 0))
		((eql fmt 'xml)
			(expr-serialize-xml expr 0))
		((eql fmt 'dot)
			(expr-serialize-dot expr))
		((eql fmt 'infix)
			(expr-serialize-infix expr))
		((eql fmt 'prefix)
			(expr-serialize-prefix expr))
		((eql fmt 'ast)
			(expr-serialize-ast expr "" :t))
		((eql fmt 'tree)
			(expr-serialize-tree expr 0))
		((eql fmt 'rainbow)
			(expr-serialize-rainbow expr 0))
		(:t (throw "Unknown output format" fmt))))

(defun expr-serialize-sexp (expr)
	; (expr-serialize-sexp expr) -> string
	;serialize to canonical S-expression
	(str expr))

(defun expr-serialize-pretty (expr indent color)
	; (expr-serialize-pretty expr indent color) -> string
	;pretty print with indentation
	(defq indent-str (pad "" (* 2 indent) "  "))
	(cond
		((num? expr)
			(cat indent-str (str expr)))
		((sym? expr)
			(cat indent-str (str expr)))
		((list? expr)
			(if (= (length expr) 0)
				(cat indent-str "()")
				(progn
					(defq result (cat indent-str "("))
					(defq op (first expr))
					(setq result (cat result (str op)))
					(if (> (length expr) 1)
						(progn
							(each (lambda (arg)
								(cond
									((or (num? arg) (sym? arg))
										(setq result (cat result " " (str arg))))
									(:t
										(setq result (cat result "\n"
											(expr-serialize-pretty arg (inc indent) color))))))
								(rest expr))
							(setq result (cat result ")")))
						(setq result (cat result ")")))
					result)))
		(:t (cat indent-str (str expr)))))

(defun expr-serialize-json (expr indent)
	; (expr-serialize-json expr indent) -> string
	;serialize to JSON format
	(defq indent-str (pad "" (* 2 indent) "  "))
	(cond
		((num? expr)
			(str expr))
		((sym? expr)
			(cat "\"" (str expr) "\""))
		((list? expr)
			(if (= (length expr) 0)
				"[]"
				(progn
					(defq op (first expr) args (rest expr))
					(if (= (length args) 0)
						(cat "{\"op\":" (expr-serialize-json op indent) "}")
						(progn
							(defq result (cat "{\n"
								indent-str "  \"op\": " (expr-serialize-json op indent) ",\n"
								indent-str "  \"args\": ["))
							(defq first-arg :t)
							(each (lambda (arg)
								(unless first-arg
									(setq result (cat result ",")))
								(setq result (cat result "\n"
									indent-str "    " (expr-serialize-json arg (+ indent 2))))
								(setq first-arg :nil))
								args)
							(setq result (cat result "\n" indent-str "  ]\n" indent-str "}"))
							result)))))
		(:t (str expr))))

(defun expr-serialize-xml (expr indent)
	; (expr-serialize-xml expr indent) -> string
	;serialize to XML format
	(defq indent-str (pad "" (* 2 indent) "  "))
	(cond
		((num? expr)
			(cat indent-str "<number>" (str expr) "</number>"))
		((sym? expr)
			(cat indent-str "<symbol>" (str expr) "</symbol>"))
		((list? expr)
			(if (= (length expr) 0)
				(cat indent-str "<list/>")
				(progn
					(defq op (first expr) args (rest expr))
					(defq result (cat indent-str "<expr>\n"
						indent-str "  <op>" (str op) "</op>"))
					(when (> (length args) 0)
						(setq result (cat result "\n" indent-str "  <args>"))
						(each (lambda (arg)
							(setq result (cat result "\n"
								(expr-serialize-xml arg (+ indent 2)))))
							args)
						(setq result (cat result "\n" indent-str "  </args>")))
					(setq result (cat result "\n" indent-str "</expr>"))
					result)))
		(:t (cat indent-str "<value>" (str expr) "</value>"))))

(defun expr-serialize-dot (expr)
	; (expr-serialize-dot expr) -> string
	;serialize to GraphViz DOT format
	(defq node-id 0 edges (list) nodes (list))
	(defun add-node (expr)
		(defq id node-id)
		(setq node-id (inc node-id))
		(cond
			((num? expr)
				(push nodes (cat "  n" (str id) " [label=\"" (str expr)
					"\", shape=box, style=filled, fillcolor=lightblue];"))
				id)
			((sym? expr)
				(push nodes (cat "  n" (str id) " [label=\"" (str expr)
					"\", shape=ellipse, style=filled, fillcolor=lightgreen];"))
				id)
			((list? expr)
				(defq op (first expr))
				(push nodes (cat "  n" (str id) " [label=\"" (str op)
					"\", shape=diamond, style=filled, fillcolor=lightyellow];"))
				(each (lambda (arg)
					(defq child-id (add-node arg))
					(push edges (cat "  n" (str id) " -> n" (str child-id) ";")))
					(rest expr))
				id)
			(:t
				(push nodes (cat "  n" (str id) " [label=\"" (str expr) "\"];"))
				id)))
	(add-node expr)
	(cat "digraph expr {\n"
		"  rankdir=TB;\n"
		"  node [fontname=\"Courier\"];\n"
		(join nodes "\n") "\n"
		(if (> (length edges) 0) (cat (join edges "\n") "\n") "")
		"}"))

(defun expr-serialize-infix (expr)
	; (expr-serialize-infix expr) -> string
	;serialize to infix notation with proper precedence
	(defun precedence (op)
		(cond
			((some (const eql) (list '+ '-) op) 1)
			((some (const eql) (list '* '/ '%) op) 2)
			((eql op '^) 3)
			(:t 0)))
	(defun needs-parens (parent-op child-expr)
		(if (list? child-expr)
			(< (precedence (first child-expr)) (precedence parent-op))
			:nil))
	(cond
		((num? expr) (str expr))
		((sym? expr) (str expr))
		((list? expr)
			(if (= (length expr) 0)
				"()"
				(progn
					(defq op (first expr) args (rest expr))
					(cond
						((= (length args) 0)
							(str op))
						((= (length args) 1)
							(cat (str op) " " (expr-serialize-infix (first args))))
						((= (length args) 2)
							(defq left (first args) right (second args))
							(defq left-str (if (needs-parens op left)
								(cat "(" (expr-serialize-infix left) ")")
								(expr-serialize-infix left)))
							(defq right-str (if (needs-parens op right)
								(cat "(" (expr-serialize-infix right) ")")
								(expr-serialize-infix right)))
							(cat left-str " " (str op) " " right-str))
						(:t
							(cat "(" (str op) " "
								(join (map (const expr-serialize-infix) args) " ") ")"))))))
		(:t (str expr))))

(defun expr-serialize-prefix (expr)
	; (expr-serialize-prefix expr) -> string
	;serialize to prefix notation
	(cond
		((num? expr) (str expr))
		((sym? expr) (str expr))
		((list? expr)
			(if (= (length expr) 0)
				"()"
				(cat (str (first expr)) " "
					(join (map (const expr-serialize-prefix) (rest expr)) " "))))
		(:t (str expr))))

(defun expr-serialize-ast (expr prefix last)
	; (expr-serialize-ast expr prefix last) -> string
	;serialize as ASCII art tree
	(defq connector (if last "└── " "├── "))
	(defq extension (if last "    " "│   "))
	(cond
		((num? expr)
			(cat prefix connector "[Num] " (str expr)))
		((sym? expr)
			(cat prefix connector "[Sym] " (str expr)))
		((list? expr)
			(if (= (length expr) 0)
				(cat prefix connector "[Empty]")
				(progn
					(defq op (first expr) args (rest expr))
					(defq result (cat prefix connector "[Op] " (str op)))
					(defq i 0 len (length args))
					(each (lambda (arg)
						(defq is-last (= (inc i) len))
						(setq result (cat result "\n"
							(expr-serialize-ast arg (cat prefix extension) is-last))))
						args)
					result)))
		(:t (cat prefix connector "[Val] " (str expr)))))

(defun expr-serialize-tree (expr indent)
	; (expr-serialize-tree expr indent) -> string
	;serialize as hierarchical tree view
	(defq indent-str (pad "" (* 2 indent) "  "))
	(cond
		((num? expr)
			(cat indent-str "• " (str expr)))
		((sym? expr)
			(cat indent-str "• " (str expr)))
		((list? expr)
			(if (= (length expr) 0)
				(cat indent-str "• ()")
				(progn
					(defq op (first expr) args (rest expr))
					(defq result (cat indent-str "▸ " (str op)))
					(each (lambda (arg)
						(setq result (cat result "\n"
							(expr-serialize-tree arg (inc indent)))))
						args)
					result)))
		(:t (cat indent-str "• " (str expr)))))

(defun expr-serialize-rainbow (expr indent)
	; (expr-serialize-rainbow expr indent) -> string
	;rainbow colored pretty print (ANSI colors)
	(defq colors (list 31 32 33 34 35 36)) ;red, green, yellow, blue, magenta, cyan
	(defq color (elem-get colors (% indent (length colors))))
	(defq indent-str (pad "" (* 2 indent) "  "))
	(cond
		((num? expr)
			(cat indent-str "\x1b[" (str color) "m" (str expr) "\x1b[0m"))
		((sym? expr)
			(cat indent-str "\x1b[" (str color) "m" (str expr) "\x1b[0m"))
		((list? expr)
			(if (= (length expr) 0)
				(cat indent-str "\x1b[" (str color) "m()\x1b[0m")
				(progn
					(defq result (cat indent-str "\x1b[" (str color) "m(\x1b[0m"))
					(defq op (first expr))
					(setq result (cat result "\x1b[" (str color) "m" (str op) "\x1b[0m"))
					(if (> (length expr) 1)
						(progn
							(each (lambda (arg)
								(cond
									((or (num? arg) (sym? arg))
										(setq result (cat result " "
											(expr-serialize-rainbow arg indent))))
									(:t
										(setq result (cat result "\n"
											(expr-serialize-rainbow arg (inc indent)))))))
								(rest expr))
							(setq result (cat result "\x1b[" (str color) "m)\x1b[0m")))
						(setq result (cat result "\x1b[" (str color) "m)\x1b[0m")))
					result)))
		(:t (cat indent-str "\x1b[" (str color) "m" (str expr) "\x1b[0m"))))

;export module functions
(export (env-peek)
	'(expr-serialize expr-serialize-sexp expr-serialize-pretty
		expr-serialize-json expr-serialize-xml expr-serialize-dot
		expr-serialize-infix expr-serialize-prefix expr-serialize-ast
		expr-serialize-tree expr-serialize-rainbow))
(env-pop)
