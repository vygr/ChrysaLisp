;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Serialization
; Converts UI tree metadata back to Lisp source code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/streams/string.inc")

(defun designer-indent (level)
	; (designer-indent level) -> str
	(defq result "")
	(times level (setq result (cat result (ascii-char 9))))
	result)

(defun designer-format-props (props)
	; (designer-format-props props) -> str
	(if (or (not props) (empty? props))
		""
		(progn
			(defq result "(")
			(each! 0 -1 1 (lambda (prop)
				(cond
					;symbol
					((and (lst? prop) (= (length prop) 1) (starts-with ":" (elem-get prop 0)))
						(setq result (cat result (elem-get prop 0))))
					;key-value pair
					((lst? prop)
						(setq result (cat result
							(elem-get prop 0) " "
							(if (str? (elem-get prop 1))
								(cat "\"" (elem-get prop 1) "\"")
								(str (elem-get prop 1))))))
					;direct value
					(:t (setq result (cat result (str prop)))))
				;add space if not last
				(when (< (!) (dec (length props)))
					(setq result (cat result " ")))) props)
			(cat result ")"))))

(defun designer-serialize-element (element level)
	; (designer-serialize-element element level) -> str
	(defq type (get :type element)
		name (get :name element)
		constructor (get :constructor element)
		props (first (get :props element))
		children (get :children element)
		indent (designer-indent level)
		result "")

	;format the macro call
	(setq result (cat indent "(" type " " name))

	;add props if present
	(when props
		(setq result (cat result " " (designer-format-props props))))

	;handle children
	(cond
		;no children - close immediately
		((empty? children)
			(setq result (cat result ")")))
		;has children - format with body
		(:t
			(setq result (cat result (ascii-char 10)))
			(each (lambda (child)
				(setq result (cat result
					(designer-serialize-element child (inc level))
					(ascii-char 10)))) children)
			(setq result (cat result indent ")"))))

	result)

(defun designer-serialize-tree (tree)
	; (designer-serialize-tree tree) -> str
	(if tree
		(designer-serialize-element tree 0)
		""))

(defun designer-save-to-file (tree filepath)
	; (designer-save-to-file tree filepath) -> :t | :nil
	(when tree
		(defq source (designer-serialize-tree tree))
		(when (defq stream (file-stream filepath +file_open_write))
			(write-line stream source)
			:t)))

(defun designer-load-app (filepath)
	; (designer-load-app filepath) -> tree | :nil
	; Loads an app file and executes it with designer tracking
	(designer-reset)
	(when (file-stream filepath)
		;load the file - this will execute with designer tracking
		(import filepath)
		(designer-get-tree)))
