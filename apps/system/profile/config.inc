;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "profile.tre"))

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:x 0
		:y 0))

(defun config-load ()
	(if (defq stream (file-stream *config_file*))
		(setq *config* (tree-load stream)))
	(if (or (not *config*) (/= (. *config* :find :version) *config_version*))
		(setq *config* (config-default))))

(defun config-update ()
	(bind '(x y) (. *window* :get_pos))
	(scatter *config* :x x :y y))

(defun config-save ()
	(config-update)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))
