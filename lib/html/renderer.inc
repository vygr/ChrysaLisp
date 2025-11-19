;; HTML Text Renderer
;; Renders HTML DOM to text output

(import "lib/class/class.inc")
(import "lib/html/dom.inc")

(defclass html-renderer (&optional _method) :nil
	; HTML text renderer
	(def this
		:document :nil
		:output ""
		:line_width 80
		:indent_level 0
		:current_line ""
		:current_line_width 0
		:in_pre :nil)

	(defmethod :init (doc &optional width)
		(def this
			:document doc
			:output ""
			:line_width (if width width 80)
			:indent_level 0
			:current_line ""
			:current_line_width 0
			:in_pre :nil)
		this)

	(defmethod :add-text (text)
		; Add text to current line
		(if (get :in_pre this)
			; In preformatted text, don't wrap
			(def this :output (cat (get :output this) text))
			; Normal text - handle wrapping
			(progn
				(defq words (split text " "))
				(each! (lambda (word)
					(when (> (length word) 0)
						(defq word-len (length word))
						(defq curr-width (get :current_line_width this))
						(defq line-width (get :line_width this))
						(defq indent-level (get :indent_level this))
						(when (> (+ curr-width word-len 1)
								(- line-width (* indent-level 2)))
							; Need to wrap
							(. this :flush-line)
							(setq curr-width 0))
						(defq curr-line (get :current_line this))
						(if (eql (get :current_line_width this) 0)
							(progn
								(def this :current_line word)
								(def this :current_line_width word-len))
							(progn
								(def this :current_line (cat curr-line " " word))
								(def this :current_line_width (+ curr-width word-len 1))))))
					words))))

	(defmethod :flush-line ()
		; Flush current line to output
		(defq curr-line (get :current_line this))
		(when (> (length curr-line) 0)
			(defq indent-level (get :indent_level this))
			(defq indent (apply cat (map (lambda (_) "  ") (range indent-level))))
			(def this :output (cat (get :output this) indent curr-line "\n"))
			(def this :current_line "")
			(def this :current_line_width 0)))

	(defmethod :add-newline ()
		; Add a newline
		(. this :flush-line)
		(def this :output (cat (get :output this) "\n")))

	(defmethod :render-node (node)
		; Render a DOM node
		(defq node-type (get :node_type node))
		(cond
			((eql node-type NODE_TEXT)
				(. this :add-text (. node :get-text)))

			((eql node-type NODE_COMMENT)
				; Skip comments in text rendering
				:nil)

			((eql node-type NODE_ELEMENT)
				(defq tag-name (. node :get-tag-name))

				; Handle different tags
				(cond
					((or (eql tag-name "h1") (eql tag-name "h2") (eql tag-name "h3")
						 (eql tag-name "h4") (eql tag-name "h5") (eql tag-name "h6"))
						(. this :flush-line)
						(. this :add-newline)
						(def this :indent_level (inc (get :indent_level this)))
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(def this :indent_level (dec (get :indent_level this)))
						(. this :flush-line)
						(. this :add-newline))

					((eql tag-name "p")
						(. this :flush-line)
						(. this :add-newline)
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(. this :flush-line)
						(. this :add-newline))

					((eql tag-name "br")
						(. this :flush-line))

					((eql tag-name "pre")
						(. this :flush-line)
						(def this :in_pre :t)
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(def this :in_pre :nil)
						(. this :flush-line)
						(. this :add-newline))

					((or (eql tag-name "ul") (eql tag-name "ol"))
						(. this :flush-line)
						(. this :add-newline)
						(def this :indent_level (inc (get :indent_level this)))
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(def this :indent_level (dec (get :indent_level this)))
						(. this :add-newline))

					((eql tag-name "li")
						(. this :flush-line)
						(defq indent-level (get :indent_level this))
						(def this :output (cat (get :output this)
							(apply cat (map (lambda (_) "  ") (range indent-level)))
							"* "))
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(. this :flush-line))

					((or (eql tag-name "div") (eql tag-name "section") (eql tag-name "article"))
						(. this :flush-line)
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(. this :flush-line))

					((eql tag-name "a")
						; Render link with URL if available
						(defq href (. node :get-attribute "href"))
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(when href
							(. this :add-text (cat " [" href "]"))))

					((or (eql tag-name "strong") (eql tag-name "b"))
						(. this :add-text "*")
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(. this :add-text "*"))

					((or (eql tag-name "em") (eql tag-name "i"))
						(. this :add-text "_")
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node))
						(. this :add-text "_"))

					; Default: just render children
					(:t
						(each! (lambda (child) (. this :render-node child))
							(get :child_nodes node)))))

			; Document node
			((eql node-type NODE_DOCUMENT)
				(each! (lambda (child) (. this :render-node child))
					(get :child_nodes node)))))

	(defmethod :render ()
		; Render the document
		(. this :render-node (get :document this))
		(. this :flush-line)
		(get :output this)))

;; Helper function to render HTML to text
(defun render-html-to-text (document &optional width)
	; Render HTML document to text
	(defq renderer (html-renderer))
	(. renderer :init document width)
	(. renderer :render))
