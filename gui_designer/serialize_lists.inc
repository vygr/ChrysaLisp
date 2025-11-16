;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Serialization for List-Based Tree Structure
; Converts list trees back to Lisp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-tree-serialize (tree)
	; Serialize a list-based tree: (id type name children)
	(if tree
		(list-tree-serialize-element tree 0)
		""))

(defun list-tree-serialize-element (elem level)
	; elem is (id type name children)
	(defq id (elem-get elem 0)
		  type (elem-get elem 1)
		  name (elem-get elem 2)
		  children (elem-get elem 3))

	; Create indentation
	(defq indent (if (> level 0)
		(apply cat (map (lambda (_) "  ") (range 0 level)))
		""))

	; Serialize this element
	(defq code (cat indent "(" type " " name))

	; Add children if any
	(if (> (length children) 0)
		(progn
			(setq code (cat code "\n"))
			(each (lambda (child)
				(setq code (cat code (list-tree-serialize-element child (inc level)) "\n")))
				children)
			(setq code (cat code indent ")")))
		(setq code (cat code ")")))

	code)

(print "List-based serialization loaded")
