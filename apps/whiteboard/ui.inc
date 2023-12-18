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
	(setq *stroke_col* (elem-get (radio-select *ink_toolbar* (- *id* +event_black)) *palette*)))

(defun action-mode ()
	(setq *stroke_mode* (+ (radio-select *mode_toolbar* (- *id* +event_pen)) +event_pen)))

(defun action-radius ()
	(setq *stroke_radius* (elem-get (radio-select *radius_toolbar* (- *id* +event_radius1)) *radiuss*)))

(defun action-style ()
	(def (. mybackdrop :dirty) :style
		(elem-get (radio-select *style_toolbar* (- *id* +event_grid)) '(:nil :grid :lines :axis))))

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
		(list (elem-get +select_picker select) "Save Whiteboard..." "." "")))

(defun action-load ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :nil *picker_mbox*
		(open-child "apps/files/child.lisp" +kn_call_open))
		(list (elem-get +select_picker select) "Load Whiteboard..." "." ".cwb")))
