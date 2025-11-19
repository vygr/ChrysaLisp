;; HTML Parser
;; Parses HTML and builds a DOM tree

(import "lib/class/class.inc")
(import "lib/html/tokenizer.inc")
(import "lib/html/dom.inc")

;; Void elements (self-closing tags)
(defq html-void-elements
	'("area" "base" "br" "col" "embed" "hr" "img" "input"
	  "link" "meta" "param" "source" "track" "wbr"))

(defun is-void-element? (tag-name)
	; Check if tag is a void element
	(defq result :nil)
	(each! (lambda (t)
		(when (eql t tag-name)
			(setq result :t)))
		html-void-elements)
	result)

(defclass html-parser (&optional _method) :nil
	; HTML parser
	(def this
		:tokenizer :nil
		:document :nil
		:current_node :nil
		:stack (list))

	(defmethod :init (html-string)
		(defq tok (html-tokenizer))
		(. tok :init html-string)
		(defq doc (html-document))
		(. doc :init)
		(def this
			:tokenizer tok
			:document doc
			:current_node doc
			:stack (list))
		this)

	(defmethod :parse ()
		; Parse HTML and return document
		(defq tok (get :tokenizer this))
		(defq continue :t)
		(while (and (get :has_more tok) continue)
			(defq token (. tok :next-token))
			(defq token-type (first token))
			(defq token-data (second token))
			(defq token-attrs (third token))

			(cond
				((eql token-type TOKEN_START_TAG)
					(. this :handle-start-tag token-data token-attrs))
				((eql token-type TOKEN_END_TAG)
					(. this :handle-end-tag token-data))
				((eql token-type TOKEN_TEXT)
					(. this :handle-text token-data))
				((eql token-type TOKEN_COMMENT)
					(. this :handle-comment token-data))
				((eql token-type TOKEN_DOCTYPE)
					; Ignore DOCTYPE for now
					:nil)
				((eql token-type TOKEN_EOF)
					(setq continue :nil))))

		(get :document this))

	(defmethod :handle-start-tag (tag-name attrs)
		; Handle start tag
		(defq doc (get :document this))
		(defq element (. doc :create-element tag-name))

		; Set attributes
		(when attrs
			(each! (lambda ((k v))
				(. element :set-attribute k v))
				(tolist attrs)))

		; Append to current node
		(defq current (get :current_node this))
		(. current :append-child element)

		; If not void element, push to stack and set as current
		(unless (is-void-element? tag-name)
			(defq stk (get :stack this))
			(push stk current)
			(def this :stack stk)
			(def this :current_node element))

		element)

	(defmethod :handle-end-tag (tag-name)
		; Handle end tag
		; Pop stack until we find matching tag
		(defq found :nil)
		(defq done :nil)
		(defq stk (get :stack this))
		(defq current (get :current_node this))
		(while (and (not (empty? stk))
				(not found)
				(not done))
			(when (eql (to-lower (get :node_name current)) tag-name)
				(setq found :t))
			(setq current (pop stk))
			(def this :current_node current)
			(def this :stack stk)
			(when found
				(setq done :t)))
		found)

	(defmethod :handle-text (text)
		; Handle text content
		; Trim whitespace-only text nodes
		(defq trimmed (trim text))
		(when (> (length trimmed) 0)
			(defq doc (get :document this))
			(defq text-node (. doc :create-text-node text))
			(defq current (get :current_node this))
			(. current :append-child text-node)))

	(defmethod :handle-comment (comment)
		; Handle comment
		(defq doc (get :document this))
		(defq comment-node (. doc :create-comment comment))
		(defq current (get :current_node this))
		(. current :append-child comment-node)))

;; Helper function to parse HTML string
(defun parse-html (html-string)
	; Parse HTML string and return document
	(defq parser (html-parser))
	(. parser :init html-string)
	(. parser :parse))
