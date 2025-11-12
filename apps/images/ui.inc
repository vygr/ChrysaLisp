;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-tree-action ()
	;any tree mutate action
	(.-> (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
		:layout :dirty_all))

(defun action-file-tree-expand ()
	;expand all tree nodes
	(. *file_tree* :expand))

(defun action-file-tree-collapse ()
	;collapse all tree nodes
	(. *file_tree* :collapse))

(defun action-file-leaf-action ()
	;load the file selected
	(win-refresh (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
