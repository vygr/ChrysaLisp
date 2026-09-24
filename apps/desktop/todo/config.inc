;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(import "service/lock/app.inc")

(defq *config* :nil +config_version 1 +config_file (cat *env_home* "todo.tre"))

(defun config-default ()
	(scatter (Emap) :version +config_version :spacing 26 :todo (list) :done (list) :deleted (list)))

(defun config-load ()
	(setq *config* (with-read-lock +config_file
		(tree-load (file-stream +config_file))))
	(if (or (not *config*) (/= (. *config* :find :version) +config_version))
		(setq *config* (config-default))))

(defun config-update ()
	(bind '(todo_items done_items deleted_items)
		(map (# (map (# (. (last (. %0 :children)) :get_text)) %0))
			(map (# (. %0 :children)) (list *todo_flow* *done_flow* *deleted_flow*))))
	(scatter *config* :todo todo_items :done done_items :deleted deleted_items))

(defun config-save ()
	(config-update)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))
