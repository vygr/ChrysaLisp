;; DOM Query and Selection API
;; Provides functions for finding and manipulating elements in DOM tree

;; Find element by tag name
(defun find-element-by-tag (root tag-name)
	; Recursively search DOM tree for element with matching tag
	; Returns first matching element or nil
	(if (and root (eql (class-name root) "html-element"))
		(progn
			(defq element-tag (. root :tag))
			(if (and element-tag (eql element-tag tag-name))
				root
				(progn
					(defq children (. root :children))
					(if children
						(progn
							(defq idx 0)
							(while (< idx (length children))
								(defq result (find-element-by-tag (elem-get children idx) tag-name))
								(if result (return result))
								(setq idx (+ idx 1))))
						:nil))))
		:nil))

;; Find all elements by tag name
(defun find-all-elements-by-tag (root tag-name &optional results)
	; Recursively search DOM tree for all elements with matching tag
	; Returns list of matching elements
	(if (not results) (setq results (list)))

	(if (and root (eql (class-name root) "html-element"))
		(progn
			(defq element-tag (. root :tag))
			(if (and element-tag (eql element-tag tag-name))
				(push results root))

			(defq children (. root :children))
			(if children
				(progn
					(defq idx 0)
					(while (< idx (length children))
						(find-all-elements-by-tag (elem-get children idx) tag-name results)
						(setq idx (+ idx 1)))))))

	results)

;; Get element text content
(defun get-text-content (element)
	; Extract all text content from element and its children
	; Returns concatenated text
	(defq text "")

	(if element
		(progn
			(if (eql (class-name element) "text-node")
				(progn
					(defq node-text (. element :text))
					(if node-text (setq text node-text)))
				:nil)

			(if (eql (class-name element) "html-element")
				(progn
					(defq children (. element :children))
					(if children
						(progn
							(defq idx 0)
							(while (< idx (length children))
								(defq child-text (get-text-content (elem-get children idx)))
								(if child-text (setq text (cat text child-text)))
								(setq idx (+ idx 1))))
						:nil))
				:nil)))

	text)

;; Count elements
(defun count-elements (root &optional count)
	; Count total number of elements in DOM tree
	(if (not count) (setq count 0))

	(if root
		(progn
			(if (eql (class-name root) "html-element")
				(setq count (+ count 1)))

			(defq children (. root :children))
			(if children
				(progn
					(defq idx 0)
					(while (< idx (length children))
						(count-elements (elem-get children idx) count)
						(setq idx (+ idx 1)))))))

	count)

;; Get element attribute
(defun get-attribute (element attr-name)
	; Get value of named attribute from element
	(if (and element (eql (class-name element) "html-element"))
		(progn
			(defq attributes (. element :attrs))
			(if attributes
				(progn
					(defq idx 0)
					(while (< idx (length attributes))
						(defq attr (elem-get attributes idx))
						(defq attr-name-val (. attr :name))
						(if (eql attr-name-val attr-name)
							(return (. attr :value)))
						(setq idx (+ idx 1))))
				:nil))
		:nil))

;; List all tag names in document
(defun list-all-tags (root &optional tags)
	; Collect all unique tag names in DOM tree
	(if (not tags) (setq tags (list)))

	(if (and root (eql (class-name root) "html-element"))
		(progn
			(defq element-tag (. root :tag))
			(if element-tag
				(if (not (member element-tag tags))
					(push tags element-tag)))

			(defq children (. root :children))
			(if children
				(progn
					(defq idx 0)
					(while (< idx (length children))
						(list-all-tags (elem-get children idx) tags)
						(setq idx (+ idx 1)))))))

	tags)

;; Check if element has children
(defun has-children? (element)
	; Returns true if element has child nodes
	(if (and element (eql (class-name element) "html-element"))
		(progn
			(defq children (. element :children))
			(and children (> (length children) 0)))
		:nil))

;; Get first child element
(defun first-child (element)
	; Returns first child element or nil
	(if (and element (eql (class-name element) "html-element"))
		(progn
			(defq children (. element :children))
			(if (and children (> (length children) 0))
				(elem-get children 0)
				:nil))
		:nil))

;; Get next sibling element
(defun next-sibling (element parent)
	; Find element in parent's children and return next sibling
	; Requires parent element to search within
	(if (and element parent)
		(progn
			(defq children (. parent :children))
			(if children
				(progn
					(defq idx 0)
					(while (< idx (length children))
						(if (eql (elem-get children idx) element)
							(if (< (+ idx 1) (length children))
								(return (elem-get children (+ idx 1)))
								(return :nil)))
						(setq idx (+ idx 1))))
				:nil))
		:nil))

