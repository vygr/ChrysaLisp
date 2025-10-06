;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "./widgets.inc")

(enums +dlist 0
	(enum mask committed_canvas overlay_canvas committed_polygons overlay_paths))

(enums +path 0
	(enum mode color radius path))

(enums +select 0
	(enum main picker timer tip))

(bits +layer 0
	(bit committed overlay))

(defq +tol 3.0
	*radiuss* (map (const n2f) '(2 6 12)) *stroke_radius* (first *radiuss*)
	*undo_stack* (list) *redo_stack* (list)
	*stroke_col* (first *palette*) *stroke_mode* +event_pen
	*committed_polygons* (list) overlay_paths (list)
	*picker_mbox* :nil *picker_mode* :nil *running* :t
	rate (/ 1000000 60) +layer_all (+ +layer_committed +layer_overlay))

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
						(list (path-gen-arc x y 0.0 +fp_2pi (vec-length (vec-sub (path x y) (path x1 y1))) (path)))))
				((= mode +event_fbox)
					;flatten to filled box
					(list (path x y x1 y x1 y1 x y1)))
				((= mode +event_fcircle)
					;flatten to filled circle
					(list (path-gen-arc x y 0.0 +fp_2pi (vec-length (vec-sub (path x y) (path x1 y1))) (path))))
				(:t ;flatten to pen stroke
					(path-stroke-polylines (list) rad +join_bevel +cap_round +cap_round (list pnts))))))))

(defun snapshot ()
	;take a snapshot of the canvas state
	(push *undo_stack* (cat *committed_polygons*))
	(clear *redo_stack*))

(defun redraw-layers (mask)
	;redraw layer/s
	(elem-set dlist +dlist_committed_polygons (cat *committed_polygons*))
	(elem-set dlist +dlist_overlay_paths (cat overlay_paths))
	(elem-set dlist +dlist_mask (logior (elem-get dlist +dlist_mask) mask)))

(defun commit (p)
	;commit a stroke to the canvas
	(push *committed_polygons* (flatten_path p)))

(defun fpoly (canvas col mode _)
	;draw a polygon on a canvas
	(. canvas :set_color col)
	(. canvas :fpoly 0.0 0.0 mode _))

(defun redraw (dlist)
	;redraw layer/s
	(when (bits? (elem-get dlist +dlist_mask) +layer_committed)
		(defq canvas (elem-get dlist +dlist_committed_canvas))
		(. canvas :fill 0)
		(each (lambda ((col poly))
			(fpoly canvas col +winding_none_zero poly)) (elem-get dlist +dlist_committed_polygons))
		(. canvas :swap 0))
	(when (bits? (elem-get dlist +dlist_mask) +layer_overlay)
		(defq canvas (elem-get dlist +dlist_overlay_canvas))
		(. canvas :fill 0)
		(each (lambda (p)
			(bind '(col poly) (flatten_path p))
			(fpoly canvas col +winding_none_zero poly)) (elem-get dlist +dlist_overlay_paths))
		(. canvas :swap 0))
	(elem-set dlist +dlist_mask 0))

;import actions and bindings
(import "./actions.inc")

(defun main ()
	(defq select (task-mboxes +select_size) *id* :t
		dlist (list +layer_all *committed_canvas* *overlay_canvas* (list) (list)))
	(. *committed_canvas* :set_canvas_flags +canvas_flag_antialias)
	(. *overlay_canvas* :set_canvas_flags +canvas_flag_antialias)
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
				(redraw dlist))
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
								:version "CWB Version 2.0"
								:polygons *committed_polygons*)))
					;load whiteboard
					(:t (when (ends-with ".cwb" *msg*)
							(bind '(version polygons)
								(gather (tree-load (file-stream *msg*))
									:version :polygons))
							(when (eql version "CWB Version 2.0")
								(snapshot)
								(setq *committed_polygons* polygons)
								(redraw-layers +layer_committed))))))
			((defq *id* (getf *msg* +ev_msg_target_id) action (. *event_map* :find *id*))
				;call bound event action
				(action))
			((and (not (Textfield? (. *window* :find_id *id*)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((bits? mod +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta)
						;call bound control/command key action
						(if (defq action (. *key_map_control* :find key))
							(action)))
					((bits? mod +ev_key_mod_shift)
						;call bound shift key action
						(if (defq action (. *key_map_shift* :find key))
							(action)))
					((defq action (. *key_map* :find key))
						;call bound key action
						(action))))
			(:t ;gui event
				(. *window* :event *msg*))))
	;close window
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(gui-sub-rpc *window*)
	(profile-report "Whiteboard App"))
