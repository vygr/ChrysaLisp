;; HTML Parser
;; Parses HTML and builds a DOM tree

(import "lib/html/tokenizer.inc")
(import "lib/html/dom.inc")

;; Void elements (self-closing tags)
(defq html-void-elements
	'("area" "base" "br" "col" "embed" "hr" "img" "input"
	  "link" "meta" "param" "source" "track" "wbr"))

(defun is-void-element? (tag-name)
	; Check if tag is a void element
	(some! 0 -1 (lambda (t) (= t tag-name)) html-void-elements))

(defclass html-parser nil
	(defq
		tokenizer nil
		document nil
		current_node nil
		stack (list)))

(defmethod :init html-parser (html-string)
	(setq this-tokenizer (html-tokenizer :init html-string)
		this-document (html-document :init)
		this-current_node this-document
		this-stack (list))
	this)

(defmethod :parse html-parser ()
	; Parse HTML and return document
	(while (. this-tokenizer :has-more)
		(defq token (. this-tokenizer :next-token))
		(defq token-type (first token))
		(defq token-data (second token))
		(defq token-attrs (third token))

		(cond
			((= token-type TOKEN_START_TAG)
				(. this :handle-start-tag token-data token-attrs))
			((= token-type TOKEN_END_TAG)
				(. this :handle-end-tag token-data))
			((= token-type TOKEN_TEXT)
				(. this :handle-text token-data))
			((= token-type TOKEN_COMMENT)
				(. this :handle-comment token-data))
			((= token-type TOKEN_DOCTYPE)
				; Ignore DOCTYPE for now
				nil)
			((= token-type TOKEN_EOF)
				(break))))

	this-document)

(defmethod :handle-start-tag html-parser (tag-name attrs)
	; Handle start tag
	(defq element (. this-document :create-element tag-name))

	; Set attributes
	(when attrs
		(each! 0 -1 (lambda ((k v))
			(. element :set-attribute k v))
			(tolist attrs)))

	; Append to current node
	(. this-current_node :append-child element)

	; If not void element, push to stack and set as current
	(unless (is-void-element? tag-name)
		(push this-stack this-current_node)
		(setq this-current_node element))

	element)

(defmethod :handle-end-tag html-parser (tag-name)
	; Handle end tag
	; Pop stack until we find matching tag
	(defq found :nil)
	(while (and (not (empty? this-stack))
			(not found))
		(when (= (lower-case (. this-current_node 'node_name)) tag-name)
			(setq found :t))
		(setq this-current_node (pop this-stack))
		(when found
			(break)))
	found)

(defmethod :handle-text html-parser (text)
	; Handle text content
	; Trim whitespace-only text nodes
	(defq trimmed (trim text))
	(when (> (length trimmed) 0)
		(defq text-node (. this-document :create-text-node text))
		(. this-current_node :append-child text-node)))

(defmethod :handle-comment html-parser (comment)
	; Handle comment
	(defq comment-node (. this-document :create-comment comment))
	(. this-current_node :append-child comment-node))

;; Helper function to parse HTML string
(defun parse-html (html-string)
	; Parse HTML string and return document
	(defq parser (html-parser :init html-string))
	(. parser :parse))
