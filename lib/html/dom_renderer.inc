;; DOM Renderer - Convert DOM Tree to Formatted Text Output
;; Renders DOM nodes with proper indentation and formatting

;; Render element opening tag
(defun render-opening-tag (element indent)
	; Render HTML opening tag with attributes
	(defq tag (. element :tag))
	(defq attrs (. element :attrs))
	(defq output (cat (slice "                                                  " 0 indent) "<" tag))

	; Add attributes if present
	(if attrs
		(progn
			(defq idx 0)
			(while (< idx (length attrs))
				(defq attr (elem-get attrs idx))
				(defq attr-name (. attr :name))
				(defq attr-value (. attr :value))
				(if (and attr-name attr-value)
					(setq output (cat output " " attr-name "=\"" attr-value "\"")))
				(setq idx (+ idx 1))))
		:nil)

	(cat output ">"))

;; Render element closing tag
(defun render-closing-tag (element indent)
	; Render HTML closing tag
	(defq tag (. element :tag))
	(cat (slice "                                                  " 0 indent) "</" tag ">"))

;; Render text node
(defun render-text-node (node indent)
	; Render text content with indentation
	(defq text (. node :text))
	(if text
		(cat (slice "                                                  " 0 indent) text)
		""))

;; Render single DOM node and children
(defun render-dom-node (node indent &optional output)
	; Recursively render DOM node and all children
	; Returns formatted string representation
	(if (not output) (setq output (list)))

	(if node
		(progn
			(defq node-type (class-name node))

			; Handle different node types
			(if (eql node-type "html-element")
				(progn
					; Render opening tag
					(push output (render-opening-tag node indent))

					; Render children
					(defq children (. node :children))
					(if children
						(progn
							(defq idx 0)
							(while (< idx (length children))
								(render-dom-node (elem-get children idx) (+ indent 2) output)
								(setq idx (+ idx 1))))
						:nil)

					; Render closing tag
					(push output (render-closing-tag node indent)))
				:nil)

			(if (eql node-type "text-node")
				(progn
					(defq text-content (render-text-node node indent))
					(if (> (length text-content) 0)
						(push output text-content)))
				:nil)

			(if (eql node-type "comment-node")
				(progn
					(defq comment (. node :text))
					(if comment
						(push output (cat (slice "                                                  " 0 indent) "<!-- " comment " -->"))))
				:nil)))

	output)

;; Render complete DOM to formatted string
(defun render-dom-tree (root)
	; Render complete DOM tree starting from root element
	(defq rendered (render-dom-node root 0))
	(defq lines (list))
	(defq idx 0)

	; Join all lines
	(while (< idx (length rendered))
		(push lines (elem-get rendered idx))
		(setq idx (+ idx 1)))

	lines)

;; Display rendered HTML
(defun display-rendered-html (lines)
	; Print rendered HTML to console
	(defq idx 0)
	(while (< idx (length lines))
		(print (elem-get lines idx))
		(setq idx (+ idx 1))))

;; Render and display DOM
(defun render-and-display (root)
	; Complete pipeline: render DOM tree and display it
	(defq rendered-lines (render-dom-tree root))
	(display-rendered-html rendered-lines)
	rendered-lines)

;; Get rendered HTML as string
(defun get-rendered-html (root)
	; Render DOM and return as single concatenated string
	(defq rendered (render-dom-node root 0))
	(defq output "")
	(defq idx 0)

	(while (< idx (length rendered))
		(setq output (cat output (elem-get rendered idx) "\n"))
		(setq idx (+ idx 1)))

	output)

;; Count text nodes
(defun count-text-nodes (node &optional count)
	; Count total text nodes in DOM tree
	(if (not count) (setq count 0))

	(if node
		(progn
			(if (eql (class-name node) "text-node")
				(setq count (+ count 1)))

			(defq children (. node :children))
			(if children
				(progn
					(defq idx 0)
					(while (< idx (length children))
						(count-text-nodes (elem-get children idx) count)
						(setq idx (+ idx 1)))))))

	count)

;; Summarize DOM tree
(defun summarize-dom-tree (root)
	; Generate summary statistics of DOM tree
	(defq element-count 0)
	(defq text-count 0)

	; Count elements (from dom_query module pattern)
	(defq traverse-count (lambda (node)
		(if node
			(progn
				(if (eql (class-name node) "html-element")
					(setq element-count (+ element-count 1)))
				(if (eql (class-name node) "text-node")
					(setq text-count (+ text-count 1)))

				(defq children (. node :children))
				(if children
					(progn
						(defq idx 0)
						(while (< idx (length children))
							((deref traverse-count) (elem-get children idx))
							(setq idx (+ idx 1)))))))))

	(funcall traverse-count root)

	(defq summary (list))
	(push summary (cat "Elements: " (str element-count)))
	(push summary (cat "Text nodes: " (str text-count)))
	(push summary (cat "Total nodes: " (str (+ element-count text-count))))

	summary)
