;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(import "service/lock/app.inc")

(defq *config* :nil +config_version 2 +config_file (cat *env_home* "chess.tre"))

(defun config-default ()
	(scatter (Emap)
		:history (list "RNBQKBNRPPPPPPPP                                pppppppprnbqkbnr    "
		:redo_stack (list) :human_color +white :color +white :version +config_version)))

(defun config-load ()
	(with-read-lock +config_file
		(if (defq stream (file-stream +config_file))
			(setq *config* (tree-load stream) stream :nil)))
	(if (or (not *config*) (/= (. *config* :find :version) +config_version))
		(setq *config* (config-default)))
	(setq history (. *config* :find :history)
		redo_stack (. *config* :find :redo_stack)
		color (. *config* :find :color)
		human_color (. *config* :find :human_color)
		brd (last history)))

(defun config-save ()
	(scatter *config* :history history :redo_stack redo_stack :color color :human_color human_color)
	(with-write-lock +config_file
		(when (defq stream (file-stream +config_file +file_open_write))
			(tree-save stream *config*)
			(stream-flush stream)
			(setq stream :nil))))
