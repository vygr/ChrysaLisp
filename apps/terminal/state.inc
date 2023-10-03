;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(defun state-save ()
	;save terminal state, merges in
	(defq default (Emap-kv :history (list)) meta_map default)
	(when (defq stream (file-stream (cat *env_home* +state_filename)))
		(catch (setq meta_map (tree-load stream))
			(progn (setq meta_map default) :t)))
	(when (defq stream (file-stream (cat *env_home* +state_filename) +file_open_write))
		(bind '(old) (gather meta_map :history))
		(bind '(current) (gather *meta_map* :history))
		(each (# (if (defq i (find %0 old))
			;old entry so pull to front
			(setq old (erase old i (inc i))))
			(push old %0)) current)
		(scatter meta_map :history old)
		(tree-save stream meta_map)))

(defun state-load ()
	;load terminal state
	(defq default (Emap-kv :history (list)))
	(setq *meta_map* default)
	(when (defq stream (file-stream (cat *env_home* +state_filename)))
		(catch (setq *meta_map* (tree-load stream))
			(progn (setq *meta_map* default) :t)))
	(bind '(history) (gather *meta_map* :history))
	(length history))
