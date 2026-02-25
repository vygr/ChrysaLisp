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
		(redraw-layers +layer_all)))

(defun action-redo ()
	;move state from redo to undo stack and restore old state
	(when (/= 0 (length *redo_stack*))
		(push *undo_stack* *committed_groups*)
		(setq *committed_groups* (pop *redo_stack*))
		(redraw-layers +layer_all)))

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

(defun action-group ()
	;merge all selected groups into one group, result stays selected
	(defq selected (filter (# (bits? (elem-get %0 2) +group_selected)) *committed_groups*)
		non_selected (filter (# (not (bits? (elem-get %0 2) +group_selected))) *committed_groups*))
	(when (nempty? selected)
		(snapshot)
		(defq all_strokes (apply cat (map second selected))
			new_group (create-group all_strokes +group_selected))
		(setq *committed_groups* (cat non_selected (list new_group)))
		(redraw-layers +layer_all)))

(defun action-ungroup ()
	;split each selected group into individual stroke groups, all stay selected
	(when (some (# (bits? (elem-get %0 2) +group_selected)) *committed_groups*)
		(snapshot)
		(setq *committed_groups*
			(apply cat (map (lambda (group)
				(if (bits? (elem-get group 2) +group_selected)
					;split: each stroke becomes its own group, marked selected
					(map (# (create-group (list %0) +group_selected)) (second group))
					;keep non-selected groups unchanged
					(list group))) *committed_groups*)))
		(redraw-layers +layer_all)))

(defun action-delete ()
	;delete all selected groups
	(when (some (# (bits? (elem-get %0 2) +group_selected)) *committed_groups*)
		(snapshot)
		(setq *committed_groups*
			(filter (# (not (bits? (elem-get %0 2) +group_selected))) *committed_groups*))
		(redraw-layers +layer_all)))

(defun align-selected (mode)
	(defq selected (filter (# (bits? (elem-get %0 2) +group_selected)) *committed_groups*))
	(when (> (length selected) 1)
		(snapshot)
		(bind '((min_x min_y) (max_x max_y)) (first (first selected)))
		(each (lambda (g)
			(bind '((gminx gminy) (gmaxx gmaxy)) (first g))
			(setq min_x (min min_x gminx) min_y (min min_y gminy)
				max_x (max max_x gmaxx) max_y (max max_y gmaxy))) (rest selected))
		(defq cx (+ min_x (/ (- max_x min_x) 2.0))
			cy (+ min_y (/ (- max_y min_y) 2.0)))
		(setq *committed_groups*
			(map (lambda (g)
				(if (bits? (elem-get g 2) +group_selected)
					(progn
						(bind '((gminx gminy) (gmaxx gmaxy)) (first g))
						(defq gcx (+ gminx (/ (- gmaxx gminx) 2.0))
							gcy (+ gminy (/ (- gmaxy gminy) 2.0))
							dx 0.0 dy 0.0)
						(case mode
							(:left (setq dx (- min_x gminx)))
							(:hcenter (setq dy (- cy gcy)))
							(:right (setq dx (- max_x gmaxx)))
							(:top (setq dy (- min_y gminy)))
							(:vcenter (setq dx (- cx gcx)))
							(:bottom (setq dy (- max_y gmaxy))))
						(if (and (= dx 0.0) (= dy 0.0))
							g
							(list
								(list (path (+ gminx dx) (+ gminy dy)) (path (+ gmaxx dx) (+ gmaxy dy)))
								(map (lambda ((col poly ((minx miny) (maxx maxy))))
									(list col
										(map (lambda (pth)
											(reduce (lambda (new_p (x y)) (push new_p (+ x dx) (+ y dy)))
												(partition pth 2) (path))) poly)
										(list (path (+ minx dx) (+ miny dy)) (path (+ maxx dx) (+ maxy dy)))))
									(second g))
								(elem-get g 2))))
					g)) *committed_groups*))
		(redraw-layers +layer_all)))

(defun action-align-left () (align-selected :left))
(defun action-align-vcenter () (align-selected :vcenter))
(defun action-align-right () (align-selected :right))
(defun action-align-top () (align-selected :top))
(defun action-align-hcenter () (align-selected :hcenter))
(defun action-align-bottom () (align-selected :bottom))

(defun action-stroke ()
	(defq strokes (. *strokes* :get_strokes) states (. *strokes* :get_states))
	(case *stroke_mode*
		(+event_move
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
				(defq hit_idx (some! (lambda ((group_bbox group_strokes group_flags))
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
					(bind '((group_bbox group_strokes group_flags) &ignore) grabbed_info)
					(defq len (length stroke)
						dx (- (n2f (elem-get stroke (- len 2))) (n2f (first stroke)))
						dy (- (n2f (elem-get stroke (- len 1))) (n2f (second stroke))))
					(bind '((gminx gminy) (gmaxx gmaxy)) group_bbox)
					(defq translated_group (list
							(list (path (+ gminx dx) (+ gminy dy)) (path (+ gmaxx dx) (+ gmaxy dy)))
							(map (lambda ((col poly ((minx miny) (maxx maxy))))
								(list col
										(map (lambda (pth)
											(reduce (lambda (new_p (x y)) (push new_p (+ x dx) (+ y dy)))
												(partition pth 2) (path))) poly)
										(list (path (+ minx dx) (+ miny dy)) (path (+ maxx dx) (+ maxy dy)))))
								group_strokes)
							group_flags))
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
		(+event_select
			; select mode - drag box to select/deselect
			(clear *staging_paths*)
			(defq commits (some (# (= (abs %0) 2)) states))
			(each (lambda (stroke state)
				(defq abs_state (abs state))
				(when (>= abs_state 1)
					(bind '(s_minx s_miny) (slice stroke 0 2))
					(defq s_maxx s_minx s_maxy s_miny)
					(each (lambda ((x y))
							(setq s_minx (min s_minx x) s_miny (min s_miny y)
								s_maxx (max s_maxx x) s_maxy (max s_maxy y)))
						  (partition stroke 2))
					(defq area (* (- s_maxx s_minx) (- s_maxy s_miny))
						  pminx (n2f s_minx) pminy (n2f s_miny)
						  pmaxx (n2f s_maxx) pmaxy (n2f s_maxy))

					(if (= abs_state 1)
						(progn
							(setq *stroke_start_time* (pii-time))
							(. *staging_canvas* :sub)
							(if (> state 0)
								(. *backdrop* :add_front *staging_canvas*)
								(. *backdrop* :add_back *staging_canvas*))
							(push *staging_paths* (list +event_box (if (> state 0) +argb_cyan +argb_magenta) 1.0
								(list (path pminx pminy pmaxx pminy pmaxx pmaxy pminx pmaxy)))))
						(progn
							(snapshot)
							(if (< area 100)
								(progn
									(defq cx (+ pminx (/ (- pmaxx pminx) 2.0))
										cy (+ pminy (/ (- pmaxy pminy) 2.0))
										cx_p (path cx cy)
										hit_idx (rsome (lambda ((group_bbox group_strokes group_flags))
											(bind '((gminx gminy) (gmaxx gmaxy)) group_bbox)
											(if (and (<= gminx cx gmaxx) (<= gminy cy gmaxy)
														(some (lambda ((col poly ((minx miny) (maxx maxy))))
																(if (and (<= minx cx maxx) (<= miny cy maxy)
																		(vector-point-in-polygon cx_p poly +winding_none_zero))
																	:t)) group_strokes))
												(!)))
											*committed_groups*))
									(if hit_idx
										(defq group (elem-get *committed_groups* hit_idx)
											  new_flags (if (> state 0)
															(logior (elem-get group 2) +group_selected)
															(logand (elem-get group 2) (lognot +group_selected)))
											  _ (elem-set *committed_groups* hit_idx
													(list (first group) (second group) new_flags)))
										(if (> state 0)
											(setq *committed_groups*
												(map (lambda (g) (list (first g) (second g) (logand (elem-get g 2) (lognot +group_selected)))) *committed_groups*)))))
								(progn
									(setq *committed_groups*
										(map (lambda (group)
											(bind '((gminx gminy) (gmaxx gmaxy)) (first group))
											(if (not (or (< gmaxx pminx) (> gminx pmaxx)
														(< gmaxy pminy) (> gminy pmaxy)))
												(list (first group) (second group)
													(if (> state 0)
														(logior (elem-get group 2) +group_selected)
														(logand (elem-get group 2) (lognot +group_selected))))
												group)) *committed_groups*))))))))
			strokes states)
			(if commits
				(progn
					(. *strokes* :clear)
					(redraw-layers +layer_all))
				(redraw-layers +layer_staging)))
		(:t
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