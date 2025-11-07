;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "todo.tre"))

(defun get-default-config ()
	(scatter (Emap)
		:version *config_version*
		:todo (list
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen and the outside"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom")
		:done (list
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean kitchen"
			"Clean toilet"
			"Clean bathroom"
			"Clean floor")
		:deleted (list
			"Shopping")))

(defun load-config ()
	(if (defq stream (file-stream *config_file*))
		(setq *config* (tree-load stream)))
	(if (or (not *config*) (/= (. *config* :find :version) *config_version*))
		(setq *config* (get-default-config))))

(defun save-config ()
	(when (defq stream (file-stream *config_file* +file_open_write))
		(bind '(todo_items done_items deleted_items)
			(map (# (map (# (. (last (. %0 :children)) :get_text)) %0))
				(map (# (. %0 :children)) (list *todo_flow* *done_flow* *deleted_flow*))))
		(scatter *config* :todo todo_items :done done_items :deleted deleted_items)
		(tree-save stream *config*)))
