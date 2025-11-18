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
		attributes (env)))

(defmethod :init dom-node (type name)
	(setq this-node_type type
		this-node_name name
		this-node_value nil
		this-parent_node nil
		this-child_nodes (list)
		this-attributes (env))
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
