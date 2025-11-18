;; HTML DevTools Inspector
;; Console logging and DOM tree display
;; Inspired by Chrome/Firefox DevTools

(import "lib/html/dom.inc")

;; DevTools Console Logger
(defclass devtools-console nil
	(defq
		log_entries (list)
		max_entries 1000))

(defmethod :init devtools-console ()
	(setq this-log_entries (list)
		this-max_entries 1000)
	this)

(defmethod :log devtools-console (level message &optional data)
	; Log a message to console
	; level: :info, :warn, :error, :debug
	(defq entry (env))
	(set-insert entry :timestamp (time))
	(set-insert entry :level level)
	(set-insert entry :message message)
	(when data
		(set-insert entry :data data))

	; Add to log entries
	(push this-log_entries entry)

	; Trim if too many entries
	(when (> (length this-log_entries) this-max_entries)
		(setq this-log_entries (slice this-log_entries 1)))

	this)

(defmethod :info devtools-console (message &optional data)
	(. this :log :info message data))

(defmethod :warn devtools-console (message &optional data)
	(. this :log :warn message data))

(defmethod :error devtools-console (message &optional data)
	(. this :log :error message data))

(defmethod :debug devtools-console (message &optional data)
	(. this :log :debug message data))

(defmethod :clear devtools-console ()
	(setq this-log_entries (list))
	this)

(defmethod :get-entries devtools-console ()
	; Get all log entries
	this-log_entries)

(defmethod :to-string devtools-console ()
	; Convert log entries to formatted string
	(defq output (list))
	(each! 0 -1 (lambda (entry)
		(defq level (get entry :level))
		(defq msg (get entry :message))
		(defq timestamp (get entry :timestamp))

		(defq level_str
			(cond
				((= level :error) "[ERROR]")
				((= level :warn) "[WARN]")
				((= level :debug) "[DEBUG]")
				(:t "[INFO]")))

		(push output (cat level_str " " msg)))
		this-log_entries)
	(apply cat (map (lambda (s) (cat s "\n")) output)))

;; DevTools DOM Tree Navigator
(defclass dom-tree-viewer nil
	(defq
		document nil
		max_depth 10))

(defmethod :init dom-tree-viewer (doc)
	(setq this-document doc
		this-max_depth 10)
	this)

(defmethod :render-tree dom-tree-viewer (&optional node depth)
	; Render DOM tree as formatted text
	(defq current_node (if node node this-document))
	(defq current_depth (if depth depth 0))

	(when (> current_depth this-max_depth)
		(ret "  ... (max depth reached)"))

	(defq indent (apply cat (map (lambda (_) "  ") (range current_depth))))

	; Format node based on type
	(defq output
		(cond
			((= (. current_node 'node_type) NODE_ELEMENT)
				(defq tag (. current_node 'node_name))
				(defq id (. current_node :get-attribute "id"))
				(defq classes (. current_node :get-attribute "class"))

				(defq attrs "")
				(when id
					(setq attrs (cat attrs " id=\"" id "\"")))
				(when classes
					(setq attrs (cat attrs " class=\"" classes "\"")))

				(cat indent "<" tag attrs ">"))

			((= (. current_node 'node_type) NODE_TEXT)
				(defq text (. current_node 'node_value))
				(defq trimmed (trim text))
				(if (> (length trimmed) 0)
					(cat indent "\"" (if (> (length trimmed) 50)
						(cat (slice trimmed 0 47) "...")
						trimmed) "\"")
					""))

			((= (. current_node 'node_type) NODE_COMMENT)
				(cat indent "<!-- comment -->"))

			(:t (cat indent "[" (. current_node 'node_type) "]"))))

	; Recursively render children
	(defq children_output (list))
	(when (< current_depth this-max_depth)
		(each! 0 -1 (lambda (child)
			(defq child_text (. this :render-tree child (inc current_depth)))
			(when (> (length child_text) 0)
				(push children_output child_text)))
			(. current_node 'child_nodes)))

	; Combine output
	(defq all_output (list output))
	(setq all_output (cat all_output children_output))

	; Join with newlines
	(apply cat (map (lambda (s) (cat s "\n")) all_output)))

(defmethod :find-element-path dom-tree-viewer (element)
	; Find path from document root to element
	; Returns list of elements from root to target
	(defq path (list))

	(defun walk-up (node)
		(when node
			(push path node)
			(when (. node 'parent_node)
				(walk-up (. node 'parent_node)))))

	(walk-up element)
	(reverse path))

(defmethod :get-element-info dom-tree-viewer (element)
	; Get detailed info about an element
	(defq info (env))
	(set-insert info :tag_name (. element 'node_name))
	(set-insert info :node_type (. element 'node_type))

	; Get attributes
	(when (= (. element 'node_type) NODE_ELEMENT)
		(set-insert info :attributes (. element 'attributes))

		; Get computed info
		(defq child_count (length (. element 'child_nodes)))
		(set-insert info :child_count child_count)

		; Get text content
		(defq text (. element :get-text-content))
		(when (> (length text) 0)
			(set-insert info :text_content (if (> (length text) 100)
				(cat (slice text 0 97) "...")
				text))))

	info)

;; Network Resource Tracker
(defclass network-tracker nil
	(defq
		resources (list)
		max_resources 100))

(defmethod :init network-tracker ()
	(setq this-resources (list)
		this-max_resources 100)
	this)

(defmethod :track-resource network-tracker (url method status)
	; Track a network resource load
	; url: file path, method: "GET", status: "OK" or "ERROR"
	(defq entry (env))
	(set-insert entry :url url)
	(set-insert entry :method method)
	(set-insert entry :status status)
	(set-insert entry :timestamp (time))

	(push this-resources entry)

	; Trim if too many
	(when (> (length this-resources) this-max_resources)
		(setq this-resources (slice this-resources 1)))

	this)

(defmethod :get-resources network-tracker ()
	this-resources)

(defmethod :clear network-tracker ()
	(setq this-resources (list))
	this)

;; DevTools Inspector (combines console, tree, and network)
(defclass devtools-inspector nil
	(defq
		console nil
		tree_viewer nil
		network nil
		document nil))

(defmethod :init devtools-inspector (doc)
	(setq this-console (devtools-console :init)
		this-tree_viewer (dom-tree-viewer :init doc)
		this-network (network-tracker :init)
		this-document doc)
	this)

(defmethod :get-console devtools-inspector ()
	this-console)

(defmethod :get-tree-viewer devtools-inspector ()
	this-tree_viewer)

(defmethod :get-network devtools-inspector ()
	this-network)

(defmethod :render-all devtools-inspector ()
	; Render complete inspector view
	(defq output (cat
		"==== CONSOLE ====\n"
		(. this-console :to-string)
		"\n"
		"==== DOM TREE ====\n"
		(. this-tree_viewer :render-tree)
		"\n"
		"==== NETWORK ====\n"
		(. this :render-network)
		"\n"))
	output)

(defmethod :render-network devtools-inspector ()
	; Render network resources
	(defq resources (. this-network :get-resources))
	(defq output "")

	(each! 0 -1 (lambda (entry)
		(defq url (get entry :url))
		(defq method (get entry :method))
		(defq status (get entry :status))
		(setq output (cat output method " " url " - " status "\n")))
		resources)

	output)

