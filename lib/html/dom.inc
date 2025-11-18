;; HTML DOM (Document Object Model) Classes
;; Simplified DOM structure for HTML parsing and rendering

;; Node types
(defq
	NODE_ELEMENT 1
	NODE_TEXT 3
	NODE_COMMENT 8
	NODE_DOCUMENT 9)

;; Base DOM Node class
(defclass dom-node nil
	(defq
		node_type nil
		node_name nil
		node_value nil
		parent_node nil
		child_nodes (list)
		attributes (env)
		event_listeners (env)
		canvas_context nil))

(defmethod :init dom-node (type name)
	(setq this-node_type type
		this-node_name name
		this-node_value nil
		this-parent_node nil
		this-child_nodes (list)
		this-attributes (env)
		this-event_listeners (env)
		this-canvas_context nil)
	this)

(defmethod :append-child dom-node (child)
	; Add a child node
	(push this-child_nodes child)
	(set (. child 'parent_node) this)
	child)

(defmethod :remove-child dom-node (child)
	; Remove a child node
	(setq this-child_nodes
		(filter (lambda (n) (/= n child)) this-child_nodes))
	(set (. child 'parent_node) nil)
	child)

(defmethod :get-attribute dom-node (name)
	; Get an attribute value
	(get this-attributes name))

(defmethod :set-attribute dom-node (name value)
	; Set an attribute value
	(set-insert this-attributes name value)
	value)

(defmethod :has-attribute dom-node (name)
	; Check if attribute exists
	(not (nil? (get this-attributes name))))

(defmethod :get-text-content dom-node ()
	; Get all text content from node and descendants
	(defq result "")
	(cond
		((= this-node_type NODE_TEXT)
			this-node_value)
		((= this-node_type NODE_ELEMENT)
			(each! 0 -1 (lambda (child)
				(setq result (cat result (. child :get-text-content))))
				this-child_nodes)
			result)
		(:t "")))

(defmethod :set-text-content dom-node (text)
	; Set text content (replaces all children with text node)
	(when (= this-node_type NODE_ELEMENT)
		; Remove all children
		(setq this-child_nodes (list))

		; Add new text node
		(when (and text (> (length text) 0))
			(defq text_node (text-node :init text))
			(. this :append-child text_node)))
	text)

(defmethod :set-inner-html dom-node (html)
	; Set innerHTML - parses HTML and replaces children
	(when (= this-node_type NODE_ELEMENT)
		; Remove all children
		(setq this-child_nodes (list))

		; Parse HTML and add as children
		(when (and html (> (length html) 0))
			; Import parser if not already available
			; We need to parse the HTML fragment
			(import "lib/html/parser.inc")

			; Parse the HTML as a fragment
			(defq fragment_doc (parse-html html))

			; Get the body or root element's children
			(defq root (. fragment_doc 'document_element))
			(when root
				; If we got a full document structure, extract body children
				(defq bodies (. fragment_doc :get-elements-by-tag-name "body"))
				(defq source_elem
					(if (> (length bodies) 0)
						(first bodies)
						root))

				; Copy all children from parsed document to this element
				(each! 0 -1 (lambda (child)
					(. this :append-child child))
					(. source_elem 'child_nodes))))
		this)
	this)

(defmethod :to-string dom-node (&optional indent)
	; Convert node to string representation
	(defq ind (if indent indent 0))
	(defq spaces (apply cat (map (lambda (_) " ") (range ind))))

	(cond
		((= this-node_type NODE_TEXT)
			(cat spaces this-node_value))
		((= this-node_type NODE_COMMENT)
			(cat spaces "<!-- " this-node_value " -->"))
		((= this-node_type NODE_ELEMENT)
			(defq result (cat spaces "<" this-node_name))
			; Add attributes
			(each! 0 -1 (lambda ((k v))
				(setq result (cat result " " k "=\"" v "\"")))
				(tolist this-attributes))
			(if (empty? this-child_nodes)
				(cat result " />")
				(progn
					(setq result (cat result ">"))
					(each! 0 -1 (lambda (child)
						(setq result (cat result "\n" (. child :to-string (+ ind 2)))))
						this-child_nodes)
					(cat result "\n" spaces "</" this-node_name ">"))))
		(:t "")))

;; HTML Element class
(defclass html-element dom-node)

(defmethod :init html-element (tag-name)
	(. (super) :init NODE_ELEMENT tag-name)
	this)

(defmethod :get-tag-name html-element ()
	this-node_name)

;; Text Node class
(defclass text-node dom-node)

(defmethod :init text-node (text)
	(. (super) :init NODE_TEXT "#text")
	(setq this-node_value text)
	this)

(defmethod :get-text text-node ()
	this-node_value)

(defmethod :set-text text-node (text)
	(setq this-node_value text)
	text)

;; Comment Node class
(defclass comment-node dom-node)

(defmethod :init comment-node (text)
	(. (super) :init NODE_COMMENT "#comment")
	(setq this-node_value text)
	this)

;; Document class
(defclass html-document dom-node)

(defmethod :init html-document ()
	(. (super) :init NODE_DOCUMENT "#document")
	this)

(defmethod :create-element html-document (tag-name)
	; Create an element node
	(html-element :init tag-name))

(defmethod :create-text-node html-document (text)
	; Create a text node
	(text-node :init text))

(defmethod :create-comment html-document (text)
	; Create a comment node
	(comment-node :init text))

(defmethod :get-element-by-id html-document (id)
	; Find element by ID (simple depth-first search)
	(defun search-node (node)
		(cond
			((and (= (. node 'node_type) NODE_ELEMENT)
				(= (. node :get-attribute "id") id))
				node)
			(:t
				(some! 0 -1 (lambda (child)
					(search-node child))
					(. node 'child_nodes)))))
	(search-node this))

(defmethod :get-elements-by-tag-name html-document (tag-name)
	; Find all elements with given tag name
	(defq results (list))
	(defun search-node (node)
		(when (and (= (. node 'node_type) NODE_ELEMENT)
				(= (lower-case (. node 'node_name)) (lower-case tag-name)))
			(push results node))
		(each! 0 -1 (lambda (child)
			(search-node child))
			(. node 'child_nodes)))
	(search-node this)
	results)

(defmethod :query-selector html-document (selector)
	; Query selector - supports simple selectors: .class, #id, tag
	(cond
		; ID selector
		((starts-with "#" selector)
			(. this :get-element-by-id (rest selector)))

		; Class selector
		((starts-with "." selector)
			(defq class-name (rest selector))
			(defq results (list))
			(defun search-node (node)
				(when (= (. node 'node_type) NODE_ELEMENT)
					(defq classes (split (. node :get-attribute "class") " "))
					(when (some! 0 -1 (lambda (c) (= c class-name)) classes)
						(push results node)))
				(each! 0 -1 (lambda (child) (search-node child))
					(. node 'child_nodes)))
			(search-node this)
			(if (> (length results) 0) (first results) nil))

		; Tag selector
		(:t
			(defq results (. this :get-elements-by-tag-name selector))
			(if (> (length results) 0) (first results) nil))))

(defmethod :write html-document (html)
	; document.write - old-school HTML addition
	; Appends HTML to the body element
	(import "lib/html/parser.inc")

	; Get or create body element
	(defq bodies (. this :get-elements-by-tag-name "body"))
	(defq body
		(if (> (length bodies) 0)
			(first bodies)
			(progn
				; Create body if it doesn't exist
				(defq new_body (. this :create-element "body"))
				(defq root this-document_element)
				(when root
					(. root :append-child new_body))
				new_body)))

	; Parse the HTML fragment
	(defq fragment_doc (parse-html html))
	(defq fragment_root (. fragment_doc 'document_element))

	; Append children from parsed HTML to body
	(when fragment_root
		; Get the source element (prefer body, fall back to root)
		(defq fragment_bodies (. fragment_doc :get-elements-by-tag-name "body"))
		(defq source_elem
			(if (> (length fragment_bodies) 0)
				(first fragment_bodies)
				fragment_root))

		; Copy all children to body
		(each! 0 -1 (lambda (child)
			(. body :append-child child))
			(. source_elem 'child_nodes)))

	this)

;; Event handling methods (added to dom-node base class)
(defmethod :add-event-listener dom-node (event-type callback)
	; Add an event listener
	; event-type: string like "click", "mousedown", etc.
	; callback: lambda to call when event fires
	(defq listeners (get this-event_listeners event-type))
	(unless listeners
		(setq listeners (list))
		(set-insert this-event_listeners event-type listeners))

	; Add callback to list
	(push listeners callback)
	(set-insert this-event_listeners event-type listeners)
	this)

(defmethod :remove-event-listener dom-node (event-type callback)
	; Remove an event listener
	(defq listeners (get this-event_listeners event-type))
	(when listeners
		(setq listeners (filter (lambda (cb) (/= cb callback)) listeners))
		(set-insert this-event_listeners event-type listeners))
	this)

(defmethod :dispatch-event dom-node (event-type event-data)
	; Dispatch an event to all listeners
	(defq listeners (get this-event_listeners event-type))
	(when listeners
		(each! 0 -1 (lambda (callback)
			; Call callback with event data
			(callback event-data))
			listeners))
	this)

;; Canvas context support
(defmethod :get-context dom-node (context-type)
	; Get canvas rendering context
	; canvas.getContext("2d")
	(when (and (= this-node_type NODE_ELEMENT)
			(= (lower-case this-node_name) "canvas")
			(= context-type "2d"))

		; Create context if it doesn't exist
		(unless this-canvas_context
			(import "lib/html/canvas_2d.inc")
			(setq this-canvas_context (canvas-2d-context :init this)))

		this-canvas_context)
	:nil)
