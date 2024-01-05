;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(mail-send reply_mbox "")
	(setq *running* :nil))

(defun action-ok ()
	(mail-send reply_mbox (. *filename* :get_text))
	(setq *running* :nil))

(defun action-exts ()
	(setq *current_exts* (. *exts* :get_text))
	(populate-files *current_root* *current_exts*))

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

(defun action-file-action ()
	(defq file (. *file_tree* :get_route
			(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))
	(.-> *filename* (:set_text file) :layout :dirty))

(defun action-file-leaf-action ()
	(defq file (. *file_tree* :get_route
			(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
		old_file (. *filename* :get_text))
	(.-> *filename* (:set_text file) :layout :dirty)
	(if (eql file old_file) (action-ok)))
