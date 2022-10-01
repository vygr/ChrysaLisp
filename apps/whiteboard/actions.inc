;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

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

(defq
event_map (fmap-kv
	+event_black action-ink
	+event_white action-ink
	+event_red action-ink
	+event_green action-ink
	+event_blue action-ink
	+event_cyan action-ink
	+event_yellow action-ink
	+event_magenta action-ink
	+event_tblack action-ink
	+event_twhite action-ink
	+event_tred action-ink
	+event_tgreen action-ink
	+event_tblue action-ink
	+event_tcyan action-ink
	+event_tyellow action-ink
	+event_tmagenta action-ink
	+event_close action-close
	+event_grid action-style
	+event_plain action-style
	+event_axis action-style
	+event_lines action-style
	+event_radius1 action-radius
	+event_radius2 action-radius
	+event_radius3 action-radius
	+event_pen action-mode
	+event_line action-mode
	+event_arrow1 action-mode
	+event_arrow2 action-mode
	+event_box action-mode
	+event_circle action-mode
	+event_fbox action-mode
	+event_fcircle action-mode
	+event_clear action-clear
	+event_undo action-undo
	+event_redo action-redo
	+event_save action-save
	+event_load action-load
	+event_min action-minimise
	+event_max action-maximise)

key_map (fmap-kv)

key_map_shift (fmap-kv)

key_map_control (fmap-kv
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	event_map key_map key_map_shift key_map_control)
(env-pop)
