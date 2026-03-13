;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/chess/config.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "chess.tre"))

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:history (list "RNBQKBNRPPPPPPPP                                pppppppprnbqkbnr")
		:redo_stack (list)
		:human_color +white
		:color +white))

(defun config-load ()
	(if (defq stream (file-stream *config_file*))
		(setq *config* (tree-load stream)))
	(if (or (not *config*) (/= (. *config* :find :version) *config_version*))
		(setq *config* (config-default)))
	(setq history (. *config* :find :history)
		redo_stack (. *config* :find :redo_stack)
		color (. *config* :find :color)
		human_color (. *config* :find :human_color)
		brd (last history)))

(defun config-save ()
	(scatter *config*
		:history history
		:redo_stack redo_stack
		:color color
		:human_color human_color)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))