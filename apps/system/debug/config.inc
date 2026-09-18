;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(import "service/lock/app.inc")
(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "debug.tre"))

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:x 0
		:y 0))

(defun config-load ()
	(lock-claim-rpc *config_file*)
	(if (defq stream (file-stream *config_file*))
		(setq *config* (tree-load stream)))
	(if (or (not *config*) (/= (. *config* :find :version) *config_version*))
		(setq *config* (config-default)))
	(lock-release-rpc *config_file*))

(defun config-update ()
	(bind '(x y) (. *window* :get_pos))
	(scatter *config* :x x :y y))

(defun config-save ()
	(lock-claim-rpc *config_file*)
	(config-update)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*))
	(lock-release-rpc *config_file*))
