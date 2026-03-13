;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/chess/ui.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-reset ()
	(if farm (. farm :close))
	(setq history (list "RNBQKBNRPPPPPPPP                                pppppppprnbqkbnr")
		redo_stack (list)
		color +white
		brd (last history)
		selected_idx :nil
		legal_boards (list))
	(display-board brd)
	(setq last_move_time 0 start_time (pii-time) farm (Farm create destroy 1)))

(defun action-undo ()
	(when (> (length history) 1)
		(if farm (. farm :close))
		(push redo_stack (pop history))
		(setq brd (last history))
		(setq color (neg color))
		(setq selected_idx :nil legal_boards (list))
		(display-board brd)
		(setq last_move_time 0 start_time (pii-time) farm (Farm create destroy 1))))

(defun action-redo ()
	(when (nempty? redo_stack)
		(if farm (. farm :close))
		(push history (pop redo_stack))
		(setq brd (last history))
		(setq color (neg color))
		(setq selected_idx :nil legal_boards (list))
		(display-board brd)
		(setq last_move_time 0 start_time (pii-time) farm (Farm create destroy 1))))