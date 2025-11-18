;; XML Stringification
;; Converts ChrysaLisp data structures to XML strings

(defun xml-escape-text (text)
	; Escape special characters in text content
	(defq result text)
	(setq result (replace result "&" "&amp;"))
	(setq result (replace result "<" "&lt;"))
	(setq result (replace result ">" "&gt;"))
	result)

(defun xml-escape-attribute (value)
	; Escape special characters in attribute values
	(defq result value)
	(setq result (replace result "&" "&amp;"))
	(setq result (replace result "<" "&lt;"))
	(setq result (replace result ">" "&gt;"))
	(setq result (replace result "\"" "&quot;"))
	(setq result (replace result "'" "&apos;"))
	result)

(defun xml-stringify-attributes (attrs)
	; Convert attributes environment to XML attribute string
	; (env :id "123" :name "test") → " id=\"123\" name=\"test\""
	(if (or (eql attrs :nil) (eql attrs nil))
		""
		(progn
			(defq result "")
			(defq keys (env-keys attrs))
			(each! 0 -1 (lambda (key)
				(defq value (get attrs key))
				(when value
					(setq result (cat result " " (str key) "=\"" (xml-escape-attribute (str value)) "\""))))
				keys)
			result)))

(defun xml-stringify (element &optional indent-level)
	; Convert element structure to XML string
	; Element structure:
	;   (env :name "tagname"
	;        :attributes (env :attr1 "val1" ...)
	;        :text "text content"
	;        :children (list child1 child2 ...))

	(when (eql indent-level nil)
		(setq indent-level 0))

	(defq name (get element :name))
	(when (eql name nil)
		(throw "Element missing :name"))

	(defq attrs (get element :attributes))
	(defq text (get element :text))
	(defq children (get element :children))

	; Build opening tag
	(defq result (cat "<" name (xml-stringify-attributes attrs)))

	; Check if self-closing (no text, no children)
	(if (and (eql text nil) (eql children nil))
		; Self-closing tag
		(cat result "/>")

		; Has content - full tag
		(progn
			(setq result (cat result ">"))

			; Add text content if present
			(when text
				(setq result (cat result (xml-escape-text (str text)))))

			; Add children if present
			(when children
				(each! 0 -1 (lambda (child)
					(setq result (cat result (xml-stringify child (+ indent-level 1)))))
					children))

			; Closing tag
			(setq result (cat result "</" name ">"))

			result)))

(defun xml-stringify-pretty (element &optional indent-level)
	; Pretty-print XML with indentation
	; Similar to xml-stringify but adds newlines and indentation

	(when (eql indent-level nil)
		(setq indent-level 0))

	(defq name (get element :name))
	(when (eql name nil)
		(throw "Element missing :name"))

	(defq attrs (get element :attributes))
	(defq text (get element :text))
	(defq children (get element :children))

	; Create indentation
	(defq indent (apply cat (map (lambda (_) "  ") (range indent-level))))

	; Build opening tag
	(defq result (cat indent "<" name (xml-stringify-attributes attrs)))

	; Check if self-closing
	(if (and (eql text nil) (eql children nil))
		(cat result "/>\n")

		(progn
			(setq result (cat result ">"))

			; If only text (no children), keep on same line
			(if (and text (eql children nil))
				(progn
					(setq result (cat result (xml-escape-text (str text)) "</" name ">\n")))

				(progn
					; Has children or complex content
					(setq result (cat result "\n"))

					; Add text if present
					(when text
						(setq result (cat result indent "  " (xml-escape-text (str text)) "\n")))

					; Add children
					(when children
						(each! 0 -1 (lambda (child)
							(setq result (cat result (xml-stringify-pretty child (+ indent-level 1)))))
							children))

					; Closing tag on new line
					(setq result (cat result indent "</" name ">\n"))))

			result)))
