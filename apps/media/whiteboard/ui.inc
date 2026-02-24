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
	(clear *committed_groups*)
	(redraw-layers +layer_committed))

(defun action-undo ()
	;move state from undo to redo stack and restore old state
	(when (/= 0 (length *undo_stack*))
		(push *redo_stack* *committed_groups*)
		(setq *committed_groups* (pop *undo_stack*))
		(redraw-layers +layer_committed)))

(defun action-redo ()
	;move state from redo to undo stack and restore old state
	(when (/= 0 (length *redo_stack*))
		(push *undo_stack* *committed_groups*)
		(setq *committed_groups* (pop *redo_stack*))
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
			(while (< (length *grabbed_groups*) (length strokes))
				(defq state (elem-get states (length *grabbed_groups*)))
				(when (= (abs state) 1)
					; reorder the staging canvas on mouse down
					(. *staging_canvas* :sub)
					(if (> state 0)
						(. *backdrop* :add_front *staging_canvas*)
						(. *backdrop* :add_back *staging_canvas*)))
				(defq new_stroke (elem-get strokes (length *grabbed_groups*))
					px (n2f (first new_stroke))
					py (n2f (second new_stroke))
					p (path px py))
				; hit test from top to bottom (reverse draw order)
				(defq hit_idx (some! (lambda ((group_bbox group_strokes))
						(bind '((gminx gminy) (gmaxx gmaxy)) group_bbox)
						(if (and (<= gminx px gmaxx) (<= gminy py gmaxy)
								(some (lambda ((col poly ((minx miny) (maxx maxy))))
										(if (and (<= minx px maxx) (<= miny py maxy)
												(vector-point-in-polygon p poly +winding_none_zero))
											:t)) group_strokes))
							(!)))
					(list *committed_groups*) :nil -1 0))
				(if hit_idx
					(progn
						; snapshot before modifying the committed layer
						(snapshot)
						(push *grabbed_groups* (list (elem-get *committed_groups* hit_idx) state))
						(setq *committed_groups* (erase *committed_groups* hit_idx (inc hit_idx)))
						(redraw-layers +layer_committed))
					(push *grabbed_groups* :nil)))
			; process translations for both overlay and committing
			(defq moving_groups (list)
				  commits (some (# (= (abs %0) 2)) states))

			(each (lambda (grabbed_info stroke state)
				(when grabbed_info
					(bind '((group_bbox group_strokes) &ignore) grabbed_info)
					(defq len (length stroke)
						dx (- (n2f (elem-get stroke (- len 2))) (n2f (first stroke)))
						dy (- (n2f (elem-get stroke (- len 1))) (n2f (second stroke))))
					(bind '((gminx gminy) (gmaxx gmaxy)) group_bbox)
					(defq translated_group (list
							(list (list (+ gminx dx) (+ gminy dy)) (list (+ gmaxx dx) (+ gmaxy dy)))
							(map (lambda ((col poly ((minx miny) (maxx maxy))))
								(list col
									  (map (lambda (pth)
										  (reduce (lambda (new_p (x y)) (push new_p (+ x dx) (+ y dy)))
											  (partition pth 2) (path))) poly)
									  (list (list (+ minx dx) (+ miny dy)) (list (+ maxx dx) (+ maxy dy)))))
								 group_strokes)))
					(if (and commits (= (abs state) 2))
						(commit-group translated_group (= state 2))
						(push moving_groups translated_group))))
				*grabbed_groups* strokes states)

			(if commits
				(progn
					(. *strokes* :clear)
					(clear *grabbed_groups* *moving_groups*)
					(redraw-layers +layer_all))
				(progn
					(setq *moving_groups* moving_groups)
					(redraw-layers +layer_staging))))
		(progn
			; a drawing mode
			(clear *staging_paths*)
			(defq commits (some (# (= (abs %0) 2)) states))
			(if commits (snapshot))

			(each (lambda (stroke state)
				(defq abs_state (abs state))
				(when (and (= abs_state 1) (<= (length stroke) 4))
					(setq *stroke_start_time* (pii-time))
					; reorder the staging canvas on mouse down
					(. *staging_canvas* :sub)
					(if (> state 0)
						(. *backdrop* :add_front *staging_canvas*)
						(. *backdrop* :add_back *staging_canvas*)))

				; Parse, smooth, and filter the stroke coordinates once
				(unless (= *stroke_mode* +event_pen)
					(if (> (length stroke) 4)
						(setq stroke (cat (slice stroke 0 2) (slice stroke -3 -1)))))
				(setq stroke (apply (const path) (map (const n2f) stroke)))
				(if (= *stroke_mode* +event_pen) (setq stroke (path-smooth stroke)))
				
				(defq path_data (list *stroke_mode* *stroke_col* *stroke_radius*
					(path-filter +tol stroke stroke)))

				; Route to the commit layer or the active staging overlay
				(if (and commits (= abs_state 2))
					(commit path_data (= state 2))
					(push *staging_paths* path_data)))
				strokes states)

			(if commits
				(progn
					(. *strokes* :clear)
					(redraw-layers +layer_all))
				(redraw-layers +layer_staging)))))