;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(import "service/lock/app.inc")
(defun config-save ()
	;save terminal state, merges in
	(defq key (cat *env_home* +state_filename)
		default (scatter (Emap) :history (list)) meta_map default)
	(with-write-lock key
		(catch (when (defq data (tree-load (file-stream key)))
			(setq meta_map data))
			(setq meta_map default))
		(bind '(old) (gather meta_map :history))
		(bind '(current) (gather *meta_map* :history))
		(each (# (if (defq i (find %0 old))
			;old entry so pull to front
			(setq old (erase old i (inc i))))
			(push old %0)) current)
		(scatter meta_map :history old
			:zoom (if (get '*window*) (get :zoom *window*) 1.0))
		(tree-save (file-stream key +file_open_write) meta_map)))

(defun config-load ()
	;load terminal state
	(defq key (cat *env_home* +state_filename)
		default (scatter (Emap) :history (list)))
	(setq *meta_map* default)
	(with-read-lock key
		(catch (when (defq data (tree-load (file-stream key)))
			(setq *meta_map* data))
			(setq *meta_map* default)))
	(bind '(history zoom) (gather *meta_map* :history :zoom))
	(when (and zoom (get '*window*))
		(def *window* :zoom zoom))
	(length history))

;;;;;;;;;;;;;;;;
; history search
;;;;;;;;;;;;;;;;

;what was last looked for in the history, and the line that was found
;for it. A line that is still the one found is a search that goes on
(defq *history_find* "" *history_found* :none)

(defun history-find (line)
	; (history-find line) -> :nil | cmd
	;the latest command of the history that has line in it, anywhere.
	;Asked again with what it gave, the one before that, and round to the
	;latest again after the oldest. :nil if none has it. Type some of a
	;command, in any order of typing and asking, and ask till it is there.
	;The Terminal and the TUI both keep their history in *meta_map*, and
	;where they are in it in *history_idx*
	(bind '(history) (gather *meta_map* :history))
	;a line that is not the one last found is a new thing to look for,
	;from the latest. Nothing is changed till something is found
	(defq going (eql line *history_found*) want (if going *history_find* line)
		from (if going (min *history_idx* (length history)) (length history))
		has (# (or (eql want "") (nempty? (substr %0 want))))
		at (some! (# (if (has %0) (!))) (list history) :nil from 0))
	;none older, so from the latest again
	(unless at (setq at (some! (# (if (has %0) (!))) (list history) :nil (length history) 0)))
	(when at
		(setq *history_find* want *history_idx* at *history_found* (elem-get history at))))
