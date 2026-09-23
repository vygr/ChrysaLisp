;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(import "service/lock/app.inc")
(defun config-save ()
	;save terminal state, merges in
	(defq key (cat *env_home* +state_filename)
		default (scatter (Emap) :history (list)) meta_map default)
	(lock-claim-rpc key)
	(when (defq stream (file-stream key))
		(catch (setq meta_map (tree-load stream))
			(progn (setq meta_map default) :t))
		(setq stream :nil))
	(when (defq stream (file-stream key +file_open_write))
		(bind '(old) (gather meta_map :history))
		(bind '(current) (gather *meta_map* :history))
		(each (# (if (defq i (find %0 old))
			;old entry so pull to front
			(setq old (erase old i (inc i))))
			(push old %0)) current)
		(scatter meta_map :history old
			:zoom (if (get '*window*) (get :zoom *window*) 1.0))
		(tree-save stream meta_map)
		(stream-flush stream)
		(setq stream :nil))
	(lock-release-rpc key))

(defun config-load ()
	;load terminal state
	(defq key (cat *env_home* +state_filename)
		default (scatter (Emap) :history (list)))
	(setq *meta_map* default)
	(lock-claim-rpc key)
	(when (defq stream (file-stream key))
		(catch (setq *meta_map* (tree-load stream))
			(progn (setq *meta_map* default) :t))
		(setq stream :nil))
	(bind '(history zoom) (gather *meta_map* :history :zoom))
	(lock-release-rpc key)
	(when (and zoom (get '*window*))
		(def *window* :zoom zoom))
	(length history))
