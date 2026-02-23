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
	(defq style (elem-get '(:plain :grid :lines :axis) (. *style_toolbar* :get_selected))
		spacing (/ (get :spacing *backdrop*) 2))
	(def (. *backdrop* :dirty) :style style)
	(case style
		(:grid (. *strokes* :set_snap spacing spacing))
		(:axis (. *strokes* :set_snap spacing spacing))
		(:lines (. *strokes* :set_snap 0 spacing))
		(:plain (. *strokes* :set_snap 0 0))))

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
				(defq state (elem-get states (length *grabbed_polygons*)))
				(when (= (abs state) 1)
					; reorder the staging canvas on mouse down
					(. *staging_canvas* :sub)
					(if (> state 0)
						(. *backdrop* :add_front *staging_canvas*)
						(. *backdrop* :add_back *staging_canvas*)))
				; hit test from top to bottom (reverse draw order)
				(defq new_stroke (elem-get strokes (length *grabbed_polygons*))
					px (n2f (first new_stroke)) py (n2f (second new_stroke))
					p (path px py))
				(defq hit_idx (some! (lambda ((col poly ((minx miny) (maxx maxy))))
						(if (and (<= minx px maxx) (<= miny py maxy)
								(vector-point-in-polygon p poly +winding_none_zero))
							(!)))
					(list *committed_polygons*) :nil -1 0))
				(if hit_idx
					(progn
						; snapshot before modifying the committed layer
						(snapshot)
						(push *grabbed_polygons* (list (elem-get *committed_polygons* hit_idx) state))
						(setq *committed_polygons* (erase *committed_polygons* hit_idx (inc hit_idx)))
						(redraw-layers +layer_committed))
					(push *grabbed_polygons* :nil)))
			; process translations for both overlay and committing
			(defq moving_polygons (list) commits (some (# (= (abs %0) 2)) states))
			(each (lambda (grabbed_info stroke state)
				(when grabbed_info
					(bind '((col poly &ignore) &ignore) grabbed_info)
					(defq len (length stroke)
						dx (- (n2f (elem-get stroke (- len 2))) (n2f (first stroke)))
						dy (- (n2f (elem-get stroke (- len 1))) (n2f (second stroke)))
						translated_poly (map (lambda (p)
							(reduce (lambda (new_p (x y)) (push new_p (+ x dx) (+ y dy)))
								(partition p 2) (path))) poly))
					(if (and commits (= (abs state) 2))
						(commit_poly col translated_poly (= state 2))
						(push moving_polygons (list col translated_poly)))))
				*grabbed_polygons* strokes states)
			(if commits
				(progn
					(. *strokes* :clear)
					(clear *grabbed_polygons*)
					(elem-set dlist +dlist_moving_polygons (list))
					(redraw-layers +layer_all))
				(progn
					(elem-set dlist +dlist_moving_polygons moving_polygons)
					(redraw-layers +layer_staging))))
		(progn
			; a drawing mode
			(clear *staging_paths*)
			(each (lambda (stroke state)
					(when (and (= (abs state) 1) (<= (length stroke) 4))
						; reorder the staging canvas on mouse down
						(. *staging_canvas* :sub)
						(if (> state 0)
							(. *backdrop* :add_front *staging_canvas*)
							(. *backdrop* :add_back *staging_canvas*)))
					(unless (= *stroke_mode* +event_pen)
						(if (> (length stroke) 4)
							(setq stroke (cat (slice stroke 0 2) (slice stroke -3 -1)))))
					(setq stroke (apply (const path) (map (const n2f) stroke)))
					(if (= *stroke_mode* +event_pen) (setq stroke (path-smooth stroke)))
					(push *staging_paths* (list *stroke_mode* *stroke_col* *stroke_radius*
						(path-filter +tol stroke stroke))))
				strokes states)
			(redraw-layers +layer_staging)
			(when (some (# (= (abs %0) 2)) states)
				;some strokes are finished
				(snapshot)
				(each (lambda (stroke state)
					(when (= (abs state) 2)
						(unless (= *stroke_mode* +event_pen)
							(if (> (length stroke) 4)
								(setq stroke (cat (slice stroke 0 2) (slice stroke -3 -1)))))
						(setq stroke (apply (const path) (map (const n2f) stroke)))
						(if (= *stroke_mode* +event_pen) (setq stroke (path-smooth stroke)))
						(commit (list *stroke_mode* *stroke_col* *stroke_radius*
							(path-filter +tol stroke stroke)) (= state 2))))
					strokes states)
				(. *strokes* :clear)
				(clear *staging_paths*)
				(redraw-layers +layer_all)
				))))
