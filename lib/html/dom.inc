;; HTML DOM (Document Object Model) Classes
;; Simplified DOM structure for HTML parsing and rendering

(import "lib/class/class.inc")

;; Node types
(defq
	NODE_ELEMENT 1
	NODE_TEXT 3
	NODE_COMMENT 8
	NODE_DOCUMENT 9)

;; Base DOM Node class
(defclass dom-node (&optional _method) :nil
	; DOM node with children, attributes, and events
	(def this
		:node_type :nil
		:node_name :nil
		:node_value :nil
		:parent_node :nil
		:child_nodes (list)
		:attributes (env)
		:event_listeners (env)
		:canvas_context :nil
		:audio_element :nil)

	(defmethod :init (type name)
		(def this
			:node_type type
			:node_name name
			:node_value :nil
			:parent_node :nil
			:child_nodes (list)
			:attributes (env)
			:event_listeners (env)
			:canvas_context :nil
			:audio_element :nil)
		this)

	(defmethod :append-child (child)
		; Add a child node
		(defq children (get :child_nodes this))
		(push children child)
		(def this :child_nodes children)
		(def child :parent_node this)
		child)

	(defmethod :remove-child (child)
		; Remove a child node
		(defq children (filter (lambda (n) (not (eql n child))) (get :child_nodes this)))
		(def this :child_nodes children)
		(def child :parent_node :nil)
		child)

	(defmethod :get-attribute (name)
		; Get an attribute value
		(get (get :attributes this) (sym name)))

	(defmethod :set-attribute (name value)
		; Set an attribute value
		(defq attrs (get :attributes this))
		(set attrs (sym name) value)
		value)

	(defmethod :has-attribute (name)
		; Check if attribute exists
		(not (nil? (get (get :attributes this) (sym name)))))

	(defmethod :get-text-content ()
		; Get all text content from node and descendants
		(defq result "")
		(defq node-type (get :node_type this))
		(cond
			((eql node-type NODE_TEXT)
				(get :node_value this))
			((eql node-type NODE_ELEMENT)
				(each! 0 -1 (lambda (child)
					(setq result (cat result (. child :get-text-content))))
					(get :child_nodes this))
				result)
			(:t "")))

	(defmethod :set-text-content (text)
		; Set text content (replaces all children with text node)
		(when (eql (get :node_type this) NODE_ELEMENT)
			; Remove all children
			(def this :child_nodes (list))

			; Add new text node
			(when (and text (> (length text) 0))
				(defq text_node (text-node))
				(. text_node :init text)
				(. this :append-child text_node)))
		text)

	(defmethod :set-inner-html (html)
		; Set innerHTML - parses HTML and replaces children
		(when (eql (get :node_type this) NODE_ELEMENT)
			; Remove all children
			(def this :child_nodes (list))

			; Parse HTML and add as children
			(when (and html (> (length html) 0))
				; Import parser if not already available
				; We need to parse the HTML fragment
				(import "lib/html/parser.inc")

				; Parse the HTML as a fragment
				(defq fragment_doc (parse-html html))

				; Get the body or root element's children
				(defq root (get :document_element fragment_doc))
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
						(get :child_nodes source_elem))))
			this)
		this)

	(defmethod :to-string (&optional indent)
		; Convert node to string representation
		(defq ind (if indent indent 0))
		(defq spaces (apply cat (map (lambda (_) " ") (range ind))))
		(defq node-type (get :node_type this))

		(cond
			((eql node-type NODE_TEXT)
				(cat spaces (get :node_value this)))
			((eql node-type NODE_COMMENT)
				(cat spaces "<!-- " (get :node_value this) " -->"))
			((eql node-type NODE_ELEMENT)
				(defq result (cat spaces "<" (get :node_name this)))
				; Add attributes
				(each! 0 -1 (lambda ((k v))
					(setq result (cat result " " k "=\"" v "\"")))
					(tolist (get :attributes this)))
				(if (empty? (get :child_nodes this))
					(cat result " />")
					(progn
						(setq result (cat result ">"))
						(each! 0 -1 (lambda (child)
							(setq result (cat result "\n" (. child :to-string (+ ind 2)))))
							(get :child_nodes this))
						(cat result "\n" spaces "</" (get :node_name this) ">"))))
			(:t "")))

	;; Event handling methods
	(defmethod :add-event-listener (event-type callback)
		; Add an event listener
		; event-type: string like "click", "mousedown", etc.
		; callback: lambda to call when event fires
		(defq event-listeners (get :event_listeners this))
		(defq listeners (get event-listeners event-type))
		(unless listeners
			(setq listeners (list))
			(set event-listeners event-type listeners))

		; Add callback to list
		(push listeners callback)
		(set event-listeners event-type listeners)
		this)

	(defmethod :remove-event-listener (event-type callback)
		; Remove an event listener
		(defq event-listeners (get :event_listeners this))
		(defq listeners (get event-listeners event-type))
		(when listeners
			(setq listeners (filter (lambda (cb) (not (eql cb callback))) listeners))
			(set event-listeners event-type listeners))
		this)

	(defmethod :dispatch-event (event-type event-data)
		; Dispatch an event to all listeners
		(defq event-listeners (get :event_listeners this))
		(defq listeners (get event-listeners event-type))
		(when listeners
			(each! 0 -1 (lambda (callback)
				; Call callback with event data
				(callback event-data))
				listeners))
		this)

	;; Canvas context support
	(defmethod :get-context (context-type)
		; Get canvas rendering context
		; canvas.getContext("2d")
		(when (and (eql (get :node_type this) NODE_ELEMENT)
				(eql (to-lower (get :node_name this)) "canvas")
				(eql context-type "2d"))

			; Create context if it doesn't exist
			(unless (get :canvas_context this)
				(import "lib/html/canvas_2d.inc")
				(defq ctx (canvas-2d-context))
				(. ctx :init this)
				(def this :canvas_context ctx))

			(get :canvas_context this))
		:nil)

	;; Audio element support
	(defmethod :play ()
		; Play audio element
		; audio.play()
		(when (and (eql (get :node_type this) NODE_ELEMENT)
				(eql (to-lower (get :node_name this)) "audio"))

			; Create audio element if it doesn't exist
			(unless (get :audio_element this)
				(import "lib/html/web_audio.inc")
				(defq src (. this :get-attribute "src"))
				(defq audio (create-html-audio src))
				(def this :audio_element audio))

			(. (get :audio_element this) :play))
		:nil)

	(defmethod :pause ()
		; Pause audio element
		; audio.pause()
		(when (and (eql (get :node_type this) NODE_ELEMENT)
				(eql (to-lower (get :node_name this)) "audio")
				(get :audio_element this))
			(. (get :audio_element this) :pause))
		:nil)

	(defmethod :load ()
		; Load audio element
		; audio.load()
		(when (and (eql (get :node_type this) NODE_ELEMENT)
				(eql (to-lower (get :node_name this)) "audio"))

			; Create audio element if it doesn't exist
			(unless (get :audio_element this)
				(import "lib/html/web_audio.inc")
				(defq src (. this :get-attribute "src"))
				(defq audio (create-html-audio src))
				(def this :audio_element audio))

			(. (get :audio_element this) :load))
		:nil)

	(defmethod :set-volume (vol)
		; Set audio volume
		; audio.volume = 0.5
		(when (and (eql (get :node_type this) NODE_ELEMENT)
				(eql (to-lower (get :node_name this)) "audio")
				(get :audio_element this))
			(. (get :audio_element this) :set-volume vol))
		:nil)

	(defmethod :get-volume ()
		; Get audio volume
		; audio.volume
		(when (and (eql (get :node_type this) NODE_ELEMENT)
				(eql (to-lower (get :node_name this)) "audio"))

			; Create audio element if it doesn't exist
			(unless (get :audio_element this)
				(import "lib/html/web_audio.inc")
				(defq src (. this :get-attribute "src"))
				(defq audio (create-html-audio src))
				(def this :audio_element audio))

			(. (get :audio_element this) :get-volume))
		1.0))

;; HTML Element class
(defclass html-element () (dom-node)
	; HTML element node

	(defmethod :init (tag-name)
		(def this
			:node_type NODE_ELEMENT
			:node_name tag-name
			:node_value :nil
			:parent_node :nil
			:child_nodes (list)
			:attributes (env)
			:event_listeners (env)
			:canvas_context :nil
			:audio_element :nil)
		this)

	(defmethod :get-tag-name ()
		(get :node_name this)))

;; Text Node class
(defclass text-node () (dom-node)
	; Text node

	(defmethod :init (text)
		(def this
			:node_type NODE_TEXT
			:node_name "#text"
			:node_value text
			:parent_node :nil
			:child_nodes (list)
			:attributes (env)
			:event_listeners (env)
			:canvas_context :nil
			:audio_element :nil)
		this)

	(defmethod :get-text ()
		(get :node_value this))

	(defmethod :set-text (text)
		(def this :node_value text)
		text))

;; Comment Node class
(defclass comment-node () (dom-node)
	; Comment node

	(defmethod :init (text)
		(def this
			:node_type NODE_COMMENT
			:node_name "#comment"
			:node_value text
			:parent_node :nil
			:child_nodes (list)
			:attributes (env)
			:event_listeners (env)
			:canvas_context :nil
			:audio_element :nil)
		this))

;; Document class
(defclass html-document () (dom-node)
	; HTML document

	(defmethod :init ()
		(def this
			:node_type NODE_DOCUMENT
			:node_name "#document"
			:node_value :nil
			:parent_node :nil
			:child_nodes (list)
			:attributes (env)
			:event_listeners (env)
			:canvas_context :nil
			:audio_element :nil)
		this)

	(defmethod :create-element (tag-name)
		; Create an element node
		(defq elem (html-element))
		(. elem :init tag-name))

	(defmethod :create-text-node (text)
		; Create a text node
		(defq node (text-node))
		(. node :init text))

	(defmethod :create-comment (text)
		; Create a comment node
		(defq node (comment-node))
		(. node :init text))

	(defmethod :get-element-by-id (id)
		; Find element by ID (simple depth-first search)
		(defq result :nil)
		(defun search-node (node)
			(when (and (not result)
					(eql (get :node_type node) NODE_ELEMENT)
					(eql (. node :get-attribute "id") id))
				(setq result node))
			(when (not result)
				(each! 0 -1 (lambda (child)
					(search-node child))
					(get :child_nodes node))))
		(search-node this)
		result)

	(defmethod :get-elements-by-tag-name (tag-name)
		; Find all elements with given tag name
		(defq results (list))
		(defun search-node (node)
			(when (and (eql (get :node_type node) NODE_ELEMENT)
					(eql (to-lower (get :node_name node)) (to-lower tag-name)))
				(push results node))
			(each! 0 -1 (lambda (child)
				(search-node child))
				(get :child_nodes node)))
		(search-node this)
		results)

	(defmethod :query-selector (selector)
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
					(when (eql (get :node_type node) NODE_ELEMENT)
						(defq classes (split (. node :get-attribute "class") " "))
						(defq found :nil)
						(each! 0 -1 (lambda (c)
							(when (eql c class-name)
								(setq found :t)))
							classes)
						(when found
							(push results node)))
					(each! 0 -1 (lambda (child) (search-node child))
						(get :child_nodes node)))
				(search-node this)
				(if (> (length results) 0) (first results) :nil))

			; Tag selector
			(:t
				(defq results (. this :get-elements-by-tag-name selector))
				(if (> (length results) 0) (first results) :nil))))

	(defmethod :write (html)
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
					(defq root (get :document_element this))
					(when root
						(. root :append-child new_body))
					new_body)))

		; Parse the HTML fragment
		(defq fragment_doc (parse-html html))
		(defq fragment_root (get :document_element fragment_doc))

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
				(get :child_nodes source_elem)))

		this))
