;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-minimise ()
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	(def *image_scroll* :min_width +canvas_width :min_height +canvas_height)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h)
	(def *image_scroll* :min_width +min_width :min_height +min_height))

(defun action-ink ()
	(setq *stroke_col* (elem-get *palette* (. *ink_toolbar* :get_selected))))

(defun action-mode ()
	(setq *stroke_mode* (+ (. *mode_toolbar* :get_selected) +event_pen)))

(defun action-radius ()
	(setq *stroke_radius* (elem-get *radiuss* (. *radius_toolbar* :get_selected))))

(defun action-style ()
	(def (. *backdrop* :dirty) :style
		(elem-get '(:plain :grid :lines :axis) (. *style_toolbar* :get_selected))))

(defun action-clear ()
	(snapshot)
	(clear *commited_polygons*)
	(redraw-layers +layer_commited))

(defun action-undo ()
	;move state from undo to redo stack and restore old state
	(when (/= 0 (length *undo_stack*))
		(push *redo_stack* *commited_polygons*)
		(setq *commited_polygons* (pop *undo_stack*))
		(redraw-layers +layer_commited)))

(defun action-redo ()
	;move state from redo to undo stack and restore old state
	(when (/= 0 (length *redo_stack*))
		(push *undo_stack* *commited_polygons*)
		(setq *commited_polygons* (pop *redo_stack*))
		(redraw-layers +layer_commited)))

(defun action-save ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :t *picker_mbox*
		(open-child "apps/files/child.lisp" +kn_call_open))
		(list (elem-get select +select_picker) "Save Whiteboard..." "." "")))

(defun action-load ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :nil *picker_mbox*
		(open-child "apps/files/child.lisp" +kn_call_open))
		(list (elem-get select +select_picker) "Load Whiteboard..." "." ".cwb")))

(defun action-stroke ()
	;display in flight strokes
	(clear overlay_paths)
	(each (# (unless (= *stroke_mode* +event_pen)
				(if (> (length %0) 4)
					(setq %0 (cat (slice %0 0 2) (slice %0 -3 -1)))))
			(setq %0 (apply (const path) (map (const n2f) %0)))
			(if (= *stroke_mode* +event_pen) (setq %0 (path-smooth %0)))
			(push overlay_paths (list *stroke_mode* *stroke_col* *stroke_radius*
				(path-filter +tol %0 %0))))
		(. *strokes* :get_strokes))
	(redraw-layers +layer_overlay)
	;commit completed strokes
	(when (some (const identity) (defq states (. *strokes* :get_states)))
		(snapshot)
		(each (# (if %1 (commit %0))) overlay_paths states)
		(clear overlay_paths)
		(redraw-layers +layer_all)
		(. *strokes* :clear)))
