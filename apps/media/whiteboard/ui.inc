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
	(setq *stroke_mode* (+ (. *mode_toolbar* :get_selected) +event_move)))

(defun action-radius ()
	(setq *stroke_radius* (elem-get *radiuss* (. *radius_toolbar* :get_selected))))

(defun action-style ()
	(def (. *backdrop* :dirty) :style
		(elem-get '(:plain :grid :lines :axis) (. *style_toolbar* :get_selected))))

(defun action-clear ()
	(snapshot)
	(clear *committed_polygons*)
	(redraw-layers +layer_committed))

(defun action-undo ()
	;move state from undo to redo stack and restore old state
	(when (/= 0 (length *undo_stack*))
		(push *redo_stack* *committed_polygons*)
		(setq *committed_polygons* (pop *undo_stack*))
		(redraw-layers +layer_committed)))

(defun action-redo ()
	;move state from redo to undo stack and restore old state
	(when (/= 0 (length *redo_stack*))
		(push *undo_stack* *committed_polygons*)
		(setq *committed_polygons* (pop *redo_stack*))
		(redraw-layers +layer_committed)))

(defun action-save ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :t *picker_mbox*
		(open-child "apps/system/files/child.lisp" +kn_call_open))
		(cat (elem-get select +select_picker) "Save Whiteboard..." "|" ".")))

(defun action-load ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :nil *picker_mbox*
		(open-child "apps/system/files/child.lisp" +kn_call_open))
		(cat (elem-get select +select_picker) "Load Whiteboard..." "|" "." "|" ".cwb")))

(defun action-stroke ()
	(defq strokes (. *strokes* :get_strokes) states (. *strokes* :get_states))
	(if (= *stroke_mode* +event_move)
		(progn
			; move mode, sync grabbed objects with new touches
			(while (< (length *grabbed_polygons*) (length strokes))
				(defq new_stroke (elem-get strokes (length *grabbed_polygons*))
					  px (n2f (first new_stroke))
					  py (n2f (second new_stroke))
					  hit_idx :nil
					  i (length *committed_polygons*))
				; hit test from top to bottom (reverse draw order)
				(while (> i 0)
					(-- i)
					(bind '(col poly bbox) (elem-get *committed_polygons* i))
					(bind '(minx miny maxx maxy) bbox)
					(when (and (<= minx px maxx) (<= miny py maxy)
							   (vector-point-in-polygon px py poly))
						(setq hit_idx i i 0)))
				(if hit_idx
					(progn
						 ; snapshot before modifying the committed layer
						(snapshot)
						(push *grabbed_polygons* (list (elem-get *committed_polygons* hit_idx) (elem-get states (length *grabbed_polygons*))))
						(setq *committed_polygons* (erase *committed_polygons* hit_idx (inc hit_idx)))
						(redraw-layers +layer_committed))
					(push *grabbed_polygons* :nil)))
			; build live translated paths for the overlay
			(defq moving_polygons (list) moving_underlay_polygons (list))
			(each (lambda (grabbed_info stroke)
				(when grabbed_info
					(bind '((col poly &ignore) state) grabbed_info)
					(defq len (length stroke)
						  dx (- (n2f (elem-get stroke (- len 2))) (n2f (first stroke)))
						  dy (- (n2f (elem-get stroke (- len 1))) (n2f (second stroke)))
						  translated_poly (list))
					(each (lambda (p)
						(defq new_p (path) i 0 l (length p))
						(while (< i l)
							(push new_p (+ (elem-get p i) dx) (+ (elem-get p (inc i)) dy))
							(setq i (+ i 2)))
						(push translated_poly new_p)) poly)
					(if (or (= state -1) (= state 2))
						(push moving_underlay_polygons (list col translated_poly))
						(push moving_polygons (list col translated_poly)))))
				*grabbed_polygons* strokes)
			(elem-set dlist +dlist_moving_polygons moving_polygons)
			(elem-set dlist +dlist_moving_underlay_polygons moving_underlay_polygons)
			(redraw-layers (+ +layer_overlay +layer_underlay))
			; commit translated objects on Mouse Up
			(when (some (lambda (s) (or (= s 2) (= s -2))) states)
				(each (lambda (grabbed_info stroke state)
					(when (and grabbed_info (or (= state 2) (= state -2)))
						(bind '((col poly &ignore) &ignore) grabbed_info)
						(defq len (length stroke)
							  dx (- (n2f (elem-get stroke (- len 2))) (n2f (first stroke)))
							  dy (- (n2f (elem-get stroke (- len 1))) (n2f (second stroke)))
							  translated_poly (list))
						(each (lambda (p)
							(defq new_p (path) i 0 l (length p))
							(while (< i l)
								(push new_p (+ (elem-get p i) dx) (+ (elem-get p (inc i)) dy))
								(setq i (+ i 2)))
							(push translated_poly new_p)) poly)
						(commit_poly col translated_poly (= state -2))))
					*grabbed_polygons* strokes states)
				(clear *grabbed_polygons*)
				(elem-set dlist +dlist_moving_polygons (list))
				(elem-set dlist +dlist_moving_underlay_polygons (list))
				(. *strokes* :clear)
				(redraw-layers +layer_all)))
		(progn
			; draw mode
			(clear *overlay_paths* *underlay_paths*)
			(each (lambda (stroke state)
					(defq p stroke)
					(unless (= *stroke_mode* +event_pen)
						(if (> (length p) 4)
							(setq p (cat (slice p 0 2) (slice p -3 -1)))))
					(setq p (apply (const path) (map (const n2f) p)))
					(if (= *stroke_mode* +event_pen) (setq p (path-smooth p)))
					(defq item (list *stroke_mode* *stroke_col* *stroke_radius* (path-filter +tol p p)))
					(if (or (= state -1) (= state 2))
						(push *underlay_paths* item)
						(push *overlay_paths* item)))
				strokes states)
			(redraw-layers (+ +layer_overlay +layer_underlay))
			(when (some (lambda (s) (or (= s 2) (= s -2))) states)
				(snapshot)
				(each (lambda (stroke state)
						(when (or (= state 2) (= state -2))
							(defq p stroke)
							(unless (= *stroke_mode* +event_pen)
								(if (> (length p) 4)
									(setq p (cat (slice p 0 2) (slice p -3 -1)))))
							(setq p (apply (const path) (map (const n2f) p)))
							(if (= *stroke_mode* +event_pen) (setq p (path-smooth p)))
							(commit (list *stroke_mode* *stroke_col* *stroke_radius* (path-filter +tol p p))
								(= state -2))))
					strokes states)
				(clear *overlay_paths* *underlay_paths*)
				(redraw-layers +layer_all)
				(. *strokes* :clear)))))
