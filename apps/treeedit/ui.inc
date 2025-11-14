;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-selected-route ()
	; Get the route of the currently selected node
	(if *selected_node*
		(. *tree_view* :get_route *selected_node*)
		""))

(defun refresh-tree ()
	; Refresh the tree display after changes
	(bind '(x y) (. *tree_view* :get_pos))
	(bind '(w h) (. *tree_view* :pref_size))
	(.-> *tree_view* (:change x y w h) :emit))

(defun action-close ()
	; Close the tree editor
	(mail-send reply_mbox :nil)
	(setq *running* :nil))

(defun action-add-folder ()
	; Add a new folder to the tree
	(defq name (. *node_name* :get_text))
	(when (nempty? name)
		(defq parent_route (get-selected-route))
		(defq new_route (if (empty? parent_route)
			(cat name "/.")
			(cat parent_route "/" name "/.")))
		(.-> *tree_view* (:add_route new_route) :expand)
		(refresh-tree)
		(.-> *node_name* (:set_text "") :layout :dirty)))

(defun action-add-item ()
	; Add a new item (leaf node) to the tree
	(defq name (. *node_name* :get_text))
	(when (nempty? name)
		(defq parent_route (get-selected-route))
		(defq new_route (if (empty? parent_route)
			name
			(cat parent_route "/" name)))
		(. *tree_view* :add_route new_route)
		(refresh-tree)
		(.-> *node_name* (:set_text "") :layout :dirty)))

(defun action-delete-node ()
	; Delete the selected node
	(when *selected_node*
		(defq parent (penv *selected_node*))
		(when parent
			(. parent :sub_child *selected_node*)
			(setq *selected_node* :nil)
			(refresh-tree))))

(defun action-rename-node ()
	; Rename the selected node
	(defq new_name (. *node_name* :get_text))
	(when (and *selected_node* (nempty? new_name))
		; Get the old route
		(defq old_route (get-selected-route))
		; Update the node text
		(def *selected_node* :text new_name)
		; Refresh display
		(.-> *selected_node* :layout :dirty)
		(refresh-tree)
		; Clear the text field
		(.-> *node_name* (:set_text "") :layout :dirty)))

(defun collect-tree-data (node &optional prefix)
	; Recursively collect tree structure as a list of routes
	(setd prefix "")
	(defq routes (list))
	(each (lambda (child)
		(when (def? :text child)
			(defq text (get :text child))
			(unless (find text "^>") ; Skip toggle buttons
				(defq route (if (empty? prefix) text (cat prefix "/" text)))
				(push routes route)
				; Check if it's a folder (has children beyond the tab structure)
				(when (and (> (length (. child :children)) 0)
						(defq tab_flow (. child :children))
						(> (length tab_flow) 1))
					; Recursively collect children
					(defq tab_child (last (. (elem 1 tab_flow) :children)))
					(when (and tab_child (. tab_child :children))
						(each! (# (push routes %0))
							(collect-tree-data tab_child (cat route))))))))
		(. node :children))
	routes)

(defun action-save-tree ()
	; Save the tree structure to a file
	(mail-send (defq picker (open-child "apps/files/child.lisp" +kn_call_open))
		(list (task-mbox) "Save Tree As" "." "tree"))
	(defq filename (mail-read (task-mbox)))
	(when (nempty? filename)
		; Collect all routes from the tree
		(defq routes (collect-tree-data *tree_view*))
		; Save to file
		(when (defq file (file-stream filename +file_open_write))
			(each (lambda (route)
				(write-line file route)) routes)
			(. file :close))))

(defun action-load-tree ()
	; Load a tree structure from a file
	(mail-send (defq picker (open-child "apps/files/child.lisp" +kn_call_open))
		(list (task-mbox) "Load Tree From" "." "tree"))
	(defq filename (mail-read (task-mbox)))
	(when (nempty? filename)
		; Clear existing tree
		(. *tree_view* :empty)
		; Load routes from file
		(when (defq file (file-stream filename +file_open_read))
			(while (defq line (read-line file))
				(when (nempty? line)
					(. *tree_view* :add_route line)))
			(. file :close))
		(.-> *tree_view* :expand)
		(refresh-tree)))

(defun action-node-action ()
	; Handle folder node click (toggle expand/collapse)
	(defq node (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
	(when node
		(setq *selected_node* node)
		; Update the name field with the selected node
		(when (def? :text node)
			(defq text (get :text node))
			(unless (find text "^>") ; Skip toggle buttons
				(.-> *node_name* (:set_text text) :layout :dirty)))))

(defun action-node-leaf-action ()
	; Handle leaf node click (selection)
	(defq node (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
	(when node
		(setq *selected_node* node)
		; Update the name field with the selected node
		(when (def? :text node)
			(.-> *node_name* (:set_text (get :text node)) :layout :dirty))))

(defun action-name-action ()
	; Handle Enter key in name field (add item by default)
	(action-add-item))
