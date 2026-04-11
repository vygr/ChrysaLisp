;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "./widgets.inc")

(enums +select 0
	(enum main picker timer tip))

(bits +layer 0
	(bit committed staging))

(defq +tol 3.0
	*radiuss* (map (const n2f) '(2 6 12)) *stroke_radius* (first *radiuss*)
	*undo_stack* (list) *redo_stack* (list)
	*stroke_col* (first *palette*) *stroke_mode* +event_pen
	*committed_groups* (list) *staging_paths* (list)
	*grabbed_groups* (list) *moving_groups* (list)
	*picker_mbox* :nil *picker_mode* :nil *running* :t
	rate (/ 1000000 60) +layer_all (+ +layer_committed +layer_staging)
	*redraw_mask* +layer_all
	*last_commit_time* 0 *group_timeout* 2000000 *stroke_start_time* 0)

(defun flatten_path ((mode col rad pnts))
	;flatten_path path to polygon
	(list col (cond
		((< (length pnts) 2)
			;a runt so nothing
			'())
		((= 2 (length pnts))
			;just a point
			(list (path-gen-arc (first pnts) (second pnts) 0.0 +fp_2pi rad (path))))
		(:t ;is a polyline draw
			(bind '(x y x1 y1 &rest _) pnts)
			(cond
				((= mode +event_arrow1)
					;flatten to arrow1
					(path-stroke-polylines (list) rad +join_bevel +cap_butt +cap_arrow (list pnts)))
				((= mode +event_arrow2)
					;flatten to arrow2
					(path-stroke-polylines (list) rad +join_bevel +cap_arrow +cap_arrow (list pnts)))
				((= mode +event_box)
					;flatten to box
					(path-stroke-polygons (list) rad +join_miter (list (path x y x1 y x1 y1 x y1))))
				((= mode +event_circle)
					;flatten to circle
					(path-stroke-polygons (list) rad +join_bevel
						(list (path-gen-arc x y 0.0 +fp_2pi (vector-length (vector-sub (path x y) (path x1 y1))) (path)))))
				((= mode +event_fbox)
					;flatten to filled box
					(list (path x y x1 y x1 y1 x y1)))
				((= mode +event_fcircle)
					;flatten to filled circle
					(list (path-gen-arc x y 0.0 +fp_2pi (vector-length (vector-sub (path x y) (path x1 y1))) (path))))
				(:t ;flatten to pen stroke
					(path-stroke-polylines (list) rad +join_bevel +cap_round +cap_round (list pnts))))))))

(defun snapshot ()
	;take a snapshot of the canvas state
	(push *undo_stack* (cat *committed_groups*))
	(clear *redo_stack*))

(defun redraw-layers (mask)
	;flag layer/s for redraw
	(setq *redraw_mask* (logior *redraw_mask* mask)))

(defun create-group (strokes flags)
	(bind '(& & (gmin gmax)) (first strokes))
	(defq gmin (cat gmin) gmax (cat gmax))
	(each! (lambda ((& & (min_v max_v)))
			(vector-min min_v gmin gmin)
			(vector-max max_v gmax gmax))
		(list strokes) 1)
	(list (list gmin gmax) strokes flags))

(defun commit-group (group front)
	;commit a group to the canvas
	(if front
		(push *committed_groups* group)
		(setq *committed_groups* (insert *committed_groups* 0 (list group)))))

(defun commit (p front)
	;commit a stroke to the canvas
	(bind '(col poly) (flatten_path p))
	(when (nempty? poly)
		(defq stroke (list col poly (vector-bounds-2d poly)))
		(if (and (< (- *stroke_start_time* *last_commit_time*) *group_timeout*) (nempty? *committed_groups*))
			(defq target_idx (if front (dec (length *committed_groups*)) 0)
				group (elem-get *committed_groups* target_idx)
				; Use cat to create a new list so we don't mutate the version in the undo stack!
				new_strokes (cat (second group) (list stroke))
				; preserve existing flags (e.g. selected) when adding stroke to existing group
				_ (elem-set *committed_groups* target_idx (create-group new_strokes (elem-get group 2))))
			(commit-group (create-group (list stroke) 0) front))
		(setq *last_commit_time* (pii-time))))

(defun fpoly (canvas col mode _)
	;draw a polygon on a canvas
	(. canvas :set_color col)
	(. canvas :fpoly 0.0 0.0 mode _))

(defun draw-group (canvas (group_bbox strokes flags))
	;draw a group's strokes and optional selection bounding box onto canvas
	(each (lambda ((col poly bbox))
		(fpoly canvas col +winding_none_zero poly)) strokes)
	(when (bits? flags +group_selected)
		(bind '((gminx gminy) (gmaxx gmaxy)) group_bbox)
		(fpoly canvas +argb_cyan +winding_none_zero
			(path-stroke-polygons (list) 1.0 +join_miter
				(list (path gminx gminy gmaxx gminy gmaxx gmaxy gminx gmaxy))))))

(defun redraw ()
	;redraw layer/s
	(when (bits? *redraw_mask* +layer_committed)
		(. *committed_canvas* :fill 0)
		(each (# (draw-group *committed_canvas* %0)) *committed_groups*)
		(. *committed_canvas* :swap +pixmap_mode_normal))
	(when (bits? *redraw_mask* +layer_staging)
		(. *staging_canvas* :fill 0)
		(each (lambda (p)
			(bind '(col poly) (flatten_path p))
			(fpoly *staging_canvas* col +winding_none_zero poly)) *staging_paths*)
		(each (# (draw-group *staging_canvas* %0)) *moving_groups*)
		(. *staging_canvas* :swap +pixmap_mode_normal))
	(setq *redraw_mask* 0))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size) *id* :t)
	(. *committed_canvas* :set_canvas_flags +canvas_flag_antialias)
	(. *staging_canvas* :set_canvas_flags +canvas_flag_antialias)
	(action-style)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	;main event loop
	(mail-timeout (elem-get select +select_timer) rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get select +select_timer) rate 0)
				(redraw))
			((= idx +select_picker)
				;save/load picker response
				(setq *msg* (trim *msg*))
				(mail-send *picker_mbox* "")
				(setq *picker_mbox* :nil)
				(cond
					;closed picker
					((eql *msg* ""))
					;save whiteboard
					(*picker_mode*
						(tree-save
							(file-stream
								(cat (slice *msg* 0 (if (defq i (rfind "." *msg*)) (dec i) -1)) ".cwb")
								+file_open_write)
							(scatter (Emap)
								:version 3
								;strip selection flags before saving
								:groups (map (lambda (g) (list (first g) (second g) 0)) *committed_groups*))))
					;load whiteboard
					(:t (when (ends-with ".cwb" *msg*)
							(bind '(version groups polygons)
								(gather (tree-load (file-stream *msg*))
									:version :groups :polygons))
							(when (>= version 2)
								(snapshot)
								(if (= version 2)
									(setq *committed_groups* (map (lambda ((col poly))
											(create-group (list (list col poly (vector-bounds-2d poly))) 0))
										(filter (lambda ((col poly)) (nempty? poly)) polygons)))
									;normalize v3 groups: ensure flags field present (default 0, never restore selection)
									(setq *committed_groups* (map (lambda (g)
											(list (first g) (second g) 0)) groups)))
								(redraw-layers +layer_committed))))))
			;must be gui event to main mailbox
			((. *window* :dispatch *msg*))
			(:t ;gui event
				(. *window* :event *msg*))))
	;close window
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(gui-sub-rpc *window*)
	(profile-report "Whiteboard App"))