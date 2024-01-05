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
	(enum mask commited_canvas overlay_canvas commited_polygons overlay_paths))

(enums +path 0
	(enum mode color radius path))

(enums +select 0
	(enum main picker timer tip))

(bits +layer 0
	(bit commited overlay))

(defq +eps 0.25 +tol 3.0
	*radiuss* (map n2f '(2 6 12)) *stroke_radius* (first *radiuss*)
	*undo_stack* (list) *redo_stack* (list)
	*stroke_col* (first *palette*) *stroke_mode* +event_pen
	*commited_polygons* (list) overlay_paths (list)
	*picker_mbox* :nil *picker_mode* :nil *running* :t
	rate (/ 1000000 60) +layer_all (+ +layer_commited +layer_overlay))

(defun flatten ((mode col rad pnts))
	;flatten path to polygon
	(list col (cond
		((< (length pnts) 2)
			;a runt so nothing
			'())
		((= 2 (length pnts))
			;just a point
			(list (path-gen-arc (first pnts) (second pnts) 0.0 +fp_2pi rad +eps (path))))
		(:t ;is a polyline draw
			(bind '(x y x1 y1 &rest _) pnts)
			(cond
				((= mode +event_arrow1)
					;flatten to arrow1
					(path-stroke-polylines (list) rad +eps +join_bevel +cap_butt +cap_arrow (list pnts)))
				((= mode +event_arrow2)
					;flatten to arrow2
					(path-stroke-polylines (list) rad +eps +join_bevel +cap_arrow +cap_arrow (list pnts)))
				((= mode +event_box)
					;flatten to box
					(path-stroke-polygons (list) rad +eps +join_miter (list (path x y x1 y x1 y1 x y1))))
				((= mode +event_circle)
					;flatten to circle
					(path-stroke-polygons (list) rad +eps +join_bevel
						(list (path-gen-arc x y 0.0 +fp_2pi (vec-length (vec-sub (path x y) (path x1 y1)))
							+eps (path)))))
				((= mode +event_fbox)
					;flatten to filled box
					(list (path x y x1 y x1 y1 x y1)))
				((= mode +event_fcircle)
					;flatten to filled circle
					(list (path-gen-arc x y 0.0 +fp_2pi (vec-length (vec-sub (path x y) (path x1 y1)))
						+eps (path))))
				(:t ;flatten to pen stroke
					(path-stroke-polylines (list) rad +eps +join_bevel +cap_round +cap_round (list pnts))))))))

(defun snapshot ()
	;take a snapshot of the canvas state
	(push *undo_stack* (cat *commited_polygons*))
	(clear *redo_stack*))

(defun redraw-layers (mask)
	;redraw layer/s
	(elem-set +dlist_commited_polygons dlist (cat *commited_polygons*))
	(elem-set +dlist_overlay_paths dlist (cat overlay_paths))
	(elem-set +dlist_mask dlist (logior (elem-get +dlist_mask dlist) mask)))

(defun commit (p)
	;commit a stroke to the canvas
	(push *commited_polygons* (flatten p)))

(defun fpoly (canvas col mode _)
	;draw a polygon on a canvas
	(. canvas :set_color col)
	(. canvas :fpoly 0.0 0.0 mode _))

(defun redraw (dlist)
	;redraw layer/s
	(when (/= 0 (logand (elem-get +dlist_mask dlist) +layer_commited))
		(defq canvas (elem-get +dlist_commited_canvas dlist))
		(. canvas :fill 0)
		(each (lambda ((col poly))
			(fpoly canvas col +winding_none_zero poly)) (elem-get +dlist_commited_polygons dlist))
		(. canvas :swap 0))
	(when (/= 0 (logand (elem-get +dlist_mask dlist) +layer_overlay))
		(defq canvas (elem-get +dlist_overlay_canvas dlist))
		(. canvas :fill 0)
		(each (lambda (p)
			(bind '(col poly) (flatten p))
			(fpoly canvas col +winding_none_zero poly)) (elem-get +dlist_overlay_paths dlist))
		(. canvas :swap 0))
	(elem-set +dlist_mask dlist 0))

;import actions and bindings
(import "./actions.inc")

(defun main ()
	(defq select (alloc-select +select_size) *id* :t
		dlist (list +layer_all *commited_canvas* *overlay_canvas* (list) (list)))
	(. *commited_canvas* :set_canvas_flags +canvas_flag_antialias)
	(. *overlay_canvas* :set_canvas_flags +canvas_flag_antialias)
	(def *window* :tip_mbox (elem-get +select_tip select))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))

	;main event loop
	(mail-timeout (elem-get +select_timer select) rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get +select_timer select) rate 0)
				(redraw dlist))
			((= idx +select_picker)
				;save/load picker responce
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
								(cat (slice 0 (if (defq i (find-rev "." *msg*)) i -1) *msg*) ".cwb")
								+file_open_write)
							(Emap-kv
								:version "CWB Version 2.0"
								:polygons *commited_polygons*)))
					;load whiteboard
					(:t (when (ends-with ".cwb" *msg*)
							(bind '(version polygons)
								(gather (tree-load (file-stream *msg*))
									:version :polygons))
							(when (eql version "CWB Version 2.0")
								(snapshot)
								(setq *commited_polygons* polygons)
								(redraw-layers +layer_commited))))))
			((defq *id* (getf *msg* +ev_msg_target_id) action (. *event_map* :find *id*))
				;call bound event action
				(action))
			((and (not (Textfield? (. *window* :find_id *id*)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta))))
						;call bound control/command key action
						(if (defq action (. *key_map_control* :find key))
							(action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action
						(if (defq action (. *key_map_shift* :find key))
							(action)))
					((defq action (. *key_map* :find key))
						;call bound key action
						(action))))
			(:t ;gui event
				(. *window* :event *msg*))))
	;close window
	(free-select select)
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(gui-sub *window*)
	(profile-report "Whiteboard App"))
