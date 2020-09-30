;imports
(import "lib/math/math.inc")
(import "apps/whiteboard/app.inc")

(structure '+event 0
	(byte 'close+ 'max+ 'min+)
	(byte 'save+ 'load+ 'clear+ 'undo+ 'redo+)
	(byte 'grid+ 'plain+ 'axis+ 'lines+)
	(byte 'radius1+ 'radius2+ 'radius3+)
	(byte 'pen+ 'line+ 'arrow1+ 'arrow2+ 'box+ 'circle+ 'fbox+ 'fcircle+)
	(byte 'black+ 'white+ 'red+ 'green+ 'blue+ 'cyan+ 'yellow+ 'magenta+
		'tblack+ 'twhite+ 'tred+ 'tgreen+ 'tblue+ 'tcyan+ 'tyellow+ 'tmagenta+))

(defun-bind trans (_)
	;transparent colour
	(+ (logand 0xffffff _) 0x60000000))

(defq canvas_width 1024 canvas_height 768 min_width 320 min_height 240 eps 0.25 tol 3.0
	radiuss (map i2f '(2 6 12)) stroke_radius (elem 0 radiuss) then (time)
	palette (list argb_black argb_white argb_red argb_green argb_blue argb_cyan argb_yellow argb_magenta)
	palette (cat palette (map trans palette)) undo_stack (list) redo_stack (list)
	stroke_col (elem 0 palette) stroke_mode +event_pen+ commited_polygons (list) overlay_paths (list)
	radius_buttons (list) style_buttons (list) ink_buttons (list) mode_buttons (list)
	picker_mbox nil picker_mode nil select (array (task-mailbox) (mail-alloc-mbox)))

(ui-window window ()
	(ui-title-bar _ "Whiteboard" (0xea19 0xea1b 0xea1a) +event_close+)
	(ui-tool-bar _ ()
		(ui-buttons (0xea07 0xe9e9 0xe970 0xe9fe 0xe99d) +event_save+)
		(ui-buttons (0xe9a3 0xe976 0xe9f0 0xe9d4) +event_grid+ () style_buttons)
		(ui-buttons (0xe979 0xe97d 0xe97b) +event_radius1+ () radius_buttons)
		(ui-buttons (0xe9ec 0xe9d8 0xe917 0xea20 0xe9f6 0xe94b 0xe960 0xe95f) +event_pen+ () mode_buttons))
	(ui-tool-bar _ (font *env_medium_toolbar_font*)
		(each (lambda (col)
			(push ink_buttons (component-connect (ui-button __ (:ink_color col :text
				(if (< _ 8) (const (num-to-utf8 0xe982)) (const (num-to-utf8 0xea04)))))
					(+ _ +event_black+)))) palette))
	(ui-scroll image_scroll (logior scroll_flag_vertical scroll_flag_horizontal)
			(:min_width canvas_width :min_height canvas_height)
		(ui-backdrop backdrop (:color 0xffF8F8FF :ink_color 0xffADD8E6 style 1)
			(ui-canvas overlay_canvas canvas_width canvas_height 1)
			(ui-canvas commited_canvas canvas_width canvas_height 1))))

(defun-bind radio-select (l i)
	;radio select buttons
	(each (lambda (b)
		(def (view-dirty b) :color (if (= _ i) (const argb_grey14) (const *env_toolbar_col*)))) l) i)

(defun-bind flatten ((mode col rad pnts))
	;flatten path to polygon
	(list col (cond
		((< (length pnts) 2)
			;a runt so nothing
			'())
		((= 2 (length pnts))
			;just a point
			(list (path-gen-arc (elem 0 pnts) (elem 1 pnts) 0.0 +fp_2pi+ rad (const eps) (path))))
		(t	;is a polyline draw
			(bind '(x y x1 y1 &rest _) pnts)
			(cond
				((= mode +event_arrow1+)
					;flatten to arrow1
					(path-stroke-polylines (list) rad (const eps) (const join_bevel) (const cap_butt) (const cap_arrow) (list pnts)))
				((= mode +event_arrow2+)
					;flatten to arrow2
					(path-stroke-polylines (list) rad (const eps) (const join_bevel) (const cap_arrow) (const cap_arrow) (list pnts)))
				((= mode +event_box+)
					;flatten to box
					(path-stroke-polygons (list) rad (const eps) (const join_miter) (list (path x y x1 y x1 y1 x y1))))
				((= mode +event_circle+)
					;flatten to circle
					(path-stroke-polygons (list) rad (const eps) (const join_bevel)
						(list (path-gen-arc x y 0.0 +fp_2pi+ (vec-length (vec-sub (path x y) (path x1 y1)))
							(const eps) (path)))))
				((= mode +event_fbox+)
					;flatten to filled box
					(list (path x y x1 y x1 y1 x y1)))
				((= mode +event_fcircle+)
					;flatten to filled circle
					(list (path-gen-arc x y 0.0 +fp_2pi+ (vec-length (vec-sub (path x y) (path x1 y1)))
						(const eps) (path))))
				(t	;flatten to pen stroke
					(path-stroke-polylines (list) rad (const eps) (const join_bevel) (const cap_round) (const cap_round) (list pnts))))))))

(defun-bind snapshot ()
	;take a snapshot of the canvas state
	(push undo_stack (cat commited_polygons))
	(clear redo_stack))

(defun-bind undo ()
	;move state from undo to redo stack and restore old state
	(when (/= 0 (length undo_stack))
		(push redo_stack commited_polygons)
		(setq commited_polygons (pop undo_stack))
		(redraw 1)))

(defun-bind redo ()
	;move state from redo to undo stack and restore old state
	(when (/= 0 (length redo_stack))
		(push undo_stack commited_polygons)
		(setq commited_polygons (pop redo_stack))
		(redraw 1)))

(defun-bind commit (p)
	;commit a stroke to the canvas
	(push commited_polygons (flatten p)))

(defun-bind redraw (mask)
	;redraw layer/s
	(elem-set +dlist_commited_polygons+ dlist (cat commited_polygons))
	(elem-set +dlist_overlay_paths+ dlist (cat overlay_paths))
	(elem-set +dlist_mask+ dlist (logior (elem +dlist_mask+ dlist) mask)))

(defun-bind main ()
	;ui tree initial setup
	(defq dlist (list 3 (/ 1000000 15) flatten commited_canvas overlay_canvas (list) (list)))
	(canvas-set-flags commited_canvas 1)
	(canvas-set-flags overlay_canvas 1)
	(view-set-size backdrop canvas_width canvas_height)
	(radio-select ink_buttons 0)
	(radio-select mode_buttons 0)
	(radio-select radius_buttons 0)
	(radio-select style_buttons 1)
	(bind '(x y w h) (apply view-locate (view-pref-size window)))
	(gui-add (view-change window x y w h))
	(def image_scroll :min_width min_width :min_height min_height)

	;create child and send args
	(mail-send dlist (defq child_mbox (open-child "apps/whiteboard/child.lisp" kn_call_open)))

	;main event loop
	(defq last_state :u last_point nil last_mid_point nil id t)
	(while id
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((/= idx 0)
				;save/load picker responce
				(mail-send "" picker_mbox)
				(setq picker_mbox nil)
				(cond
					;closed picker
					((eql msg ""))
					;save whiteboard
					(picker_mode
						(save (str (list "CWB Version 1.0" commited_polygons))
							(cat (slice 0 (if (defq i (find-rev "." msg)) i -1) msg) ".cwb")))
					;load whiteboard
					(t	(when (ends-with ".cwb" msg)
							(bind '(data _) (read (file-stream msg) (ascii-code " ")))
							(when (eql (elem 0 data) "CWB Version 1.0")
								(snapshot)
								(setq commited_polygons (map (lambda ((c p))
									(list c (map (lambda (_)
										(apply path _)) p))) (elem 1 data)))
								(redraw 1))))))
			((= (setq id (get-long msg (const ev_msg_target_id))) +event_close+)
				;close button
				(setq id nil))
			((= id +event_min+)
				;min button
				(bind '(x y w h) (apply view-fit (cat (view-get-pos window) (view-pref-size window))))
				(view-change-dirty window x y w h))
			((= id +event_max+)
				;max button
				(def image_scroll :min_width canvas_width :min_height canvas_height)
				(bind '(x y w h) (apply view-fit (cat (view-get-pos window) (view-pref-size window))))
				(view-change-dirty window x y w h)
				(def image_scroll :min_width min_width :min_height min_height))
			((<= +event_black+ id +event_tmagenta+)
				;ink pot
				(setq stroke_col (elem (radio-select ink_buttons (- id +event_black+)) palette)))
			((<= +event_pen+ id +event_fcircle+)
				;draw mode
				(setq stroke_mode (+ (radio-select mode_buttons (- id +event_pen+)) +event_pen+)))
			((<= +event_radius1+ id +event_radius3+)
				;stroke radius
				(setq stroke_radius (elem (radio-select radius_buttons (- id +event_radius1+)) radiuss)))
			((<= +event_grid+ id +event_lines+)
				;styles
				(def (view-dirty backdrop) :style (radio-select style_buttons (- id +event_grid+))))
			((= id +event_save+)
				;save
				(if picker_mbox (mail-send "" picker_mbox))
				(mail-send (list (elem -2 select) "Save Whiteboard..." "." "")
					(setq picker_mode t picker_mbox (open-child "apps/files/child.lisp" kn_call_open))))
			((= id +event_load+)
				;load
				(if picker_mbox (mail-send "" picker_mbox))
				(mail-send (list (elem -2 select) "Load Whiteboard..." "." ".cwb")
					(setq picker_mode nil picker_mbox (open-child "apps/files/child.lisp" kn_call_open))))
			((= id +event_clear+)
				;clear
				(snapshot)
				(clear commited_polygons)
				(redraw 1))
			((= id +event_undo+)
				;undo
				(undo))
			((= id +event_redo+)
				;undo
				(redo))
			((= id (component-get-id overlay_canvas))
				;event for canvas
				(when (= (get-long msg (const ev_msg_type)) (const ev_type_mouse))
					;mouse event in canvas
					(defq new_point (path (i2f (get-int msg (const ev_msg_mouse_rx)))
						(i2f (get-int msg (const ev_msg_mouse_ry)))))
					(cond
						((/= (get-int msg (const ev_msg_mouse_buttons)) 0)
							;mouse button is down
							(case last_state
								(:d	;was down last time, what draw mode ?
									(cond
										((= stroke_mode +event_pen+)
											;pen mode, so extend last stroke ?
											(defq stroke (elem +path_path+ (elem -2 overlay_paths))
												mid_vec (vec-sub new_point last_point))
											(when (>= (vec-length-squared mid_vec) (* stroke_radius stroke_radius))
												(defq mid_point (vec-add last_point (vec-scale mid_vec 0.5)))
												(path-gen-quadratic
													(elem 0 last_mid_point) (elem 1 last_mid_point)
													(elem 0 last_point) (elem 1 last_point)
													(elem 0 mid_point) (elem 1 mid_point)
													(const eps) stroke)
												(path-filter (const tol) stroke stroke)
												(setq last_point new_point last_mid_point mid_point)
												(redraw 2)))
										(t	;a shape mode
											(elem-set +path_path+ (elem -2 overlay_paths) (cat last_point new_point))
											(redraw 2)))
									)
								(:u	;was up last time, so start new stroke
									(setq last_state :d last_point new_point last_mid_point new_point)
									(push overlay_paths (list stroke_mode stroke_col stroke_radius new_point))
									(redraw 2))))
						(t	;mouse button is up
							(case last_state
								(:d	;was down last time, so last point and commit stroke
									(snapshot)
									(setq last_state :u)
									(defq stroke (elem +path_path+ (elem -2 overlay_paths)))
									(push stroke (elem 0 new_point) (elem 1 new_point))
									(path-filter 0.5 stroke stroke)
									(each commit overlay_paths)
									(clear overlay_paths)
									(redraw 3))
								(:u	;was up last time, so we are hovering
									t))))) t)
			(t (view-event window msg))))
	;close child and window
	(mail-free-mbox (elem -2 select))
	(mail-send "" child_mbox)
	(if picker_mbox (mail-send "" picker_mbox))
	(view-hide window))
