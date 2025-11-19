;; HTML DevTools Inspector
;; Console logging and DOM tree display
;; Inspired by Chrome/Firefox DevTools

(import "lib/class/class.inc")
(import "lib/html/dom.inc")

;; DevTools Console Logger
(defclass devtools-console (&optional _method) :nil
	; DevTools console
	(def this
		:log_entries (list)
		:max_entries 1000)

	(defmethod :init ()
		(def this
			:log_entries (list)
			:max_entries 1000)
		this)

	(defmethod :log (level message &optional data)
		; Log a message to console
		; level: :info, :warn, :error, :debug
		(defq entry (env))
		(set entry :timestamp (time))
		(set entry :level level)
		(set entry :message message)
		(when data
			(set entry :data data))

		; Add to log entries
		(defq entries (get :log_entries this))
		(push entries entry)
		(def this :log_entries entries)

		; Trim if too many entries
		(defq max-entries (get :max_entries this))
		(when (> (length (get :log_entries this)) max-entries)
			(def this :log_entries (slice (get :log_entries this) 1)))

		this)

	(defmethod :info (message &optional data)
		(. this :log :info message data))

	(defmethod :warn (message &optional data)
		(. this :log :warn message data))

	(defmethod :error (message &optional data)
		(. this :log :error message data))

	(defmethod :debug (message &optional data)
		(. this :log :debug message data))

	(defmethod :clear ()
		(def this :log_entries (list))
		this)

	(defmethod :get-entries ()
		; Get all log entries
		(get :log_entries this))

	(defmethod :to-string ()
		; Convert log entries to formatted string
		(defq output (list))
		(each! (lambda (entry)
			(defq level (get entry :level))
			(defq msg (get entry :message))
			(defq timestamp (get entry :timestamp))

			(defq level_str
				(cond
					((eql level :error) "[ERROR]")
					((eql level :warn) "[WARN]")
					((eql level :debug) "[DEBUG]")
					(:t "[INFO]")))

			(push output (cat level_str " " msg)))
			(get :log_entries this))
		(apply cat (map (lambda (s) (cat s "\n")) output))))

;; DevTools DOM Tree Navigator
(defclass dom-tree-viewer (&optional _method) :nil
	; DOM tree viewer
	(def this
		:document :nil
		:max_depth 10)

	(defmethod :init (doc)
		(def this
			:document doc
			:max_depth 10)
		this)

	(defmethod :render-tree (&optional node depth)
		; Render DOM tree as formatted text
		(defq current_node (if node node (get :document this)))
		(defq current_depth (if depth depth 0))

		(when (> current_depth (get :max_depth this))
			(ret "  ... (max depth reached)"))

		(defq indent (apply cat (map (lambda (_) "  ") (range current_depth))))

		; Format node based on type
		(defq output
			(cond
				((eql (get :node_type current_node) NODE_ELEMENT)
					(defq tag (get :node_name current_node))
					(defq id (. current_node :get-attribute "id"))
					(defq classes (. current_node :get-attribute "class"))

					(defq attrs "")
					(when id
						(setq attrs (cat attrs " id=\"" id "\"")))
					(when classes
						(setq attrs (cat attrs " class=\"" classes "\"")))

					(cat indent "<" tag attrs ">"))

				((eql (get :node_type current_node) NODE_TEXT)
					(defq text (get :node_value current_node))
					(defq trimmed (trim text))
					(if (> (length trimmed) 0)
						(cat indent "\"" (if (> (length trimmed) 50)
							(cat (slice trimmed 0 47) "...")
							trimmed) "\"")
						""))

				((eql (get :node_type current_node) NODE_COMMENT)
					(cat indent "<!-- comment -->"))

				(:t (cat indent "[" (get :node_type current_node) "]"))))

		; Recursively render children
		(defq children_output (list))
		(when (< current_depth (get :max_depth this))
			(each! (lambda (child)
				(defq child_text (. this :render-tree child (inc current_depth)))
				(when (> (length child_text) 0)
					(push children_output child_text)))
				(get :child_nodes current_node)))

		; Combine output
		(defq all_output (list output))
		(setq all_output (cat all_output children_output))

		; Join with newlines
		(apply cat (map (lambda (s) (cat s "\n")) all_output)))

	(defmethod :find-element-path (element)
		; Find path from document root to element
		; Returns list of elements from root to target
		(defq path (list))

		(defun walk-up (node)
			(when node
				(push path node)
				(when (get :parent_node node)
					(walk-up (get :parent_node node)))))

		(walk-up element)
		(reverse path))

	(defmethod :get-element-info (element)
		; Get detailed info about an element
		(defq info (env))
		(set info :tag_name (get :node_name element))
		(set info :node_type (get :node_type element))

		; Get attributes
		(when (eql (get :node_type element) NODE_ELEMENT)
			(set info :attributes (get :attributes element))

			; Get computed info
			(defq child_count (length (get :child_nodes element)))
			(set info :child_count child_count)

			; Get text content
			(defq text (. element :get-text-content))
			(when (> (length text) 0)
				(set info :text_content (if (> (length text) 100)
					(cat (slice text 0 97) "...")
					text))))

		info))

;; Network Resource Tracker
(defclass network-tracker (&optional _method) :nil
	; Network tracker
	(def this
		:resources (list)
		:max_resources 100)

	(defmethod :init ()
		(def this
			:resources (list)
			:max_resources 100)
		this)

	(defmethod :track-resource (url method status)
		; Track a network resource load
		; url: file path, method: "GET", status: "OK" or "ERROR"
		(defq entry (env))
		(set entry :url url)
		(set entry :method method)
		(set entry :status status)
		(set entry :timestamp (time))

		(defq resources (get :resources this))
		(push resources entry)
		(def this :resources resources)

		; Trim if too many
		(defq max-resources (get :max_resources this))
		(when (> (length (get :resources this)) max-resources)
			(def this :resources (slice (get :resources this) 1)))

		this)

	(defmethod :get-resources ()
		(get :resources this))

	(defmethod :clear ()
		(def this :resources (list))
		this))

;; DevTools Inspector (combines console, tree, and network)
(defclass devtools-inspector (&optional _method) :nil
	; DevTools inspector
	(def this
		:console :nil
		:tree_viewer :nil
		:network :nil
		:document :nil)

	(defmethod :init (doc)
		(defq cons (devtools-console))
		(. cons :init)
		(defq tree (dom-tree-viewer))
		(. tree :init doc)
		(defq net (network-tracker))
		(. net :init)
		(def this
			:console cons
			:tree_viewer tree
			:network net
			:document doc)
		this)

	(defmethod :get-console ()
		(get :console this))

	(defmethod :get-tree-viewer ()
		(get :tree_viewer this))

	(defmethod :get-network ()
		(get :network this))

	(defmethod :render-all ()
		; Render complete inspector view
		(defq cons (get :console this))
		(defq tree (get :tree_viewer this))
		(defq output (cat
			"==== CONSOLE ====\n"
			(. cons :to-string)
			"\n"
			"==== DOM TREE ====\n"
			(. tree :render-tree)
			"\n"
			"==== NETWORK ====\n"
			(. this :render-network)
			"\n"))
		output)

	(defmethod :render-network ()
		; Render network resources
		(defq net (get :network this))
		(defq resources (. net :get-resources))
		(defq output "")

		(each! (lambda (entry)
			(defq url (get entry :url))
			(defq method (get entry :method))
			(defq status (get entry :status))
			(setq output (cat output method " " url " - " status "\n")))
			resources)

		output))
