;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Enhanced Serialization for List-Based Trees
; Includes properties and constructor information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-serialize (tree)
	; Serialize enhanced tree: (id type name constructor props children)
	(if tree
		(designer-serialize-element tree 0)
		""))

(defun designer-format-props (props)
	; Format property list as Lisp syntax
	; For now, just convert to string representation
	(if (and props (nempty? props))
		(str props)
		"()"))

(defun designer-serialize-element (elem level)
	; elem is (id type name constructor props children)
	(defq id (elem-get elem 0)
		  type (elem-get elem 1)
		  name (elem-get elem 2)
		  constructor (elem-get elem 3)
		  props (elem-get elem 4)
		  children (elem-get elem 5))

	; Create indentation
	(defq indent (if (> level 0)
		(apply cat (map (lambda (_) "  ") (range 0 level)))
		""))

	; Determine what macro to use (ui-window, ui-flow, etc. instead of ui-root/ui-element)
	; For now, just use the type - we'll enhance this later
	(defq macro-name type)

	; Build the opening
	(defq code (cat indent "(" macro-name " " (str name)))

	; Add properties if present
	(when (and props (nempty? props))
		(setq code (cat code " " (designer-format-props props))))

	; Add children if any
	(if (> (length children) 0)
		(progn
			(setq code (cat code "\n"))
			(each (lambda (child)
				(setq code (cat code (designer-serialize-element child (inc level)) "\n")))
				children)
			(setq code (cat code indent ")")))
		(setq code (cat code ")")))

	code)

(print "Enhanced list-based serialization loaded")
