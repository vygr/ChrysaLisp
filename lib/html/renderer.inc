;; HTML Text Renderer
;; Renders HTML DOM to text output

(import "lib/html/dom.inc")

(defclass html-renderer nil
	(defq
		document nil
		output ""
		line_width 80
		indent_level 0
		current_line ""
		current_line_width 0
		in_pre :nil))

(defmethod :init html-renderer (doc &optional width)
	(setq this-document doc
		this-output ""
		this-line_width (if width width 80)
		this-indent_level 0
		this-current_line ""
		this-current_line_width 0
		this-in_pre :nil)
	this)

(defmethod :add-text html-renderer (text)
	; Add text to current line
	(if this-in_pre
		; In preformatted text, don't wrap
		(setq this-output (cat this-output text))
		; Normal text - handle wrapping
		(progn
			(defq words (split text " "))
			(each! 0 -1 (lambda (word)
				(when (> (length word) 0)
					(defq word-len (length word))
					(when (> (+ this-current_line_width word-len 1)
							(- this-line_width (* this-indent_level 2)))
						; Need to wrap
						(. this :flush-line))
					(if (= this-current_line_width 0)
						(setq this-current_line word
							this-current_line_width word-len)
						(setq this-current_line (cat this-current_line " " word)
							this-current_line_width (+ this-current_line_width word-len 1)))))
				words))))

(defmethod :flush-line html-renderer ()
	; Flush current line to output
	(when (> (length this-current_line) 0)
		(defq indent (apply cat (map (lambda (_) "  ") (range this-indent_level))))
		(setq this-output (cat this-output indent this-current_line "\n"))
		(setq this-current_line ""
			this-current_line_width 0)))

(defmethod :add-newline html-renderer ()
	; Add a newline
	(. this :flush-line)
	(setq this-output (cat this-output "\n")))

(defmethod :render-node html-renderer (node)
	; Render a DOM node
	(cond
		((= (. node 'node_type) NODE_TEXT)
			(. this :add-text (. node :get-text)))

		((= (. node 'node_type) NODE_COMMENT)
			; Skip comments in text rendering
			nil)

		((= (. node 'node_type) NODE_ELEMENT)
			(defq tag-name (. node :get-tag-name))

			; Handle different tags
			(cond
				((or (= tag-name "h1") (= tag-name "h2") (= tag-name "h3")
					 (= tag-name "h4") (= tag-name "h5") (= tag-name "h6"))
					(. this :flush-line)
					(. this :add-newline)
					(setq this-indent_level (inc this-indent_level))
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(setq this-indent_level (dec this-indent_level))
					(. this :flush-line)
					(. this :add-newline))

				((= tag-name "p")
					(. this :flush-line)
					(. this :add-newline)
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(. this :flush-line)
					(. this :add-newline))

				((= tag-name "br")
					(. this :flush-line))

				((= tag-name "pre")
					(. this :flush-line)
					(setq this-in_pre :t)
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(setq this-in_pre :nil)
					(. this :flush-line)
					(. this :add-newline))

				((or (= tag-name "ul") (= tag-name "ol"))
					(. this :flush-line)
					(. this :add-newline)
					(setq this-indent_level (inc this-indent_level))
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(setq this-indent_level (dec this-indent_level))
					(. this :add-newline))

				((= tag-name "li")
					(. this :flush-line)
					(setq this-output (cat this-output
						(apply cat (map (lambda (_) "  ") (range this-indent_level)))
						"* "))
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(. this :flush-line))

				((or (= tag-name "div") (= tag-name "section") (= tag-name "article"))
					(. this :flush-line)
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(. this :flush-line))

				((= tag-name "a")
					; Render link with URL if available
					(defq href (. node :get-attribute "href"))
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(when href
						(. this :add-text (cat " [" href "]"))))

				((or (= tag-name "strong") (= tag-name "b"))
					(. this :add-text "*")
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(. this :add-text "*"))

				((or (= tag-name "em") (= tag-name "i"))
					(. this :add-text "_")
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes))
					(. this :add-text "_"))

				; Default: just render children
				(:t
					(each! 0 -1 (lambda (child) (. this :render-node child))
						(. node 'child_nodes)))))

		; Document node
		((= (. node 'node_type) NODE_DOCUMENT)
			(each! 0 -1 (lambda (child) (. this :render-node child))
				(. node 'child_nodes)))))

(defmethod :render html-renderer ()
	; Render the document
	(. this :render-node this-document)
	(. this :flush-line)
	this-output)

;; Helper function to render HTML to text
(defun render-html-to-text (document &optional width)
	; Render HTML document to text
	(defq renderer (html-renderer :init document width))
	(. renderer :render))
