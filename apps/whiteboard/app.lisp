;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

(structure 'event 0
	(byte 'close 'max 'min)
	(byte 'clear 'undo 'redo)
	(byte 'radius1 'radius2 'radius3)
	(byte 'black 'white 'red 'green 'blue 'cyan 'yellow 'magenta))

(defq canvas_width 800 canvas_height 600 min_width 320 min_height 240 eps 0.25
	radiuss '(2.0 4.0 8.0) stroke_radius (elem 0 radiuss) then (time)
	palette (list argb_black argb_white argb_red argb_green argb_blue argb_cyan argb_yellow argb_magenta)
	stroke_col (elem 0 palette) commited_strokes (list) in_flight_strokes (list)
	undo_stack (list) redo_stack (list))

(ui-window window ()
	(ui-title-flow _ "Whiteboard" (0xea19 0xea1b 0xea1a) (const event_close))
	(ui-flow _ ('flow_flags (logior flow_flag_right flow_flag_fillh) 'color *env_toolbar_col* 'font *env_toolbar_font*)
		(ui-buttons (0xea31 0xe9fe 0xe99d) (const event_clear))
		(ui-buttons (0xe979 0xe97d 0xe97b) (const event_radius1))
		(each (lambda (col)
			(component-connect (ui-button __ ('color (const *env_toolbar2_col*) 'ink_color col
				'text (const (num-to-utf8 0xe95f)))) (+ _ event_black))) palette))
	(ui-scroll image_scroll (logior scroll_flag_vertical scroll_flag_horizontal)
			('min_width canvas_width 'min_height canvas_height)
		(ui-canvas canvas canvas_width canvas_height 1)))

(defun-bind flatten (r s)
	;flatten a polyline to polygons
	(cond
		((= 0 (length s))
			;a runt so nothing
			'())
		((= 2 (length s))
			;just a point
			(list (points-gen-arc (elem 0 s) (elem 1 s) 0 fp_2pi r eps (points))))
		(t	;is a polyline
			(points-stroke-polylines r eps
				(const join_round) (const cap_round) (const cap_round)
				(list s) (list)))))

(defun-bind snapshot ()
	;take a snapshot of the canvas state
	(push undo_stack (cat commited_strokes))
	(clear redo_stack))

(defun-bind undo ()
	;move state from undo to redo stack and restore old state
	(when (/= 0 (length undo_stack))
		(push redo_stack commited_strokes)
		(setq commited_strokes (pop undo_stack))
		(redraw t)))

(defun-bind redo ()
	;move state from redo to undo stack and restore old state
	(when (/= 0 (length redo_stack))
		(push undo_stack commited_strokes)
		(setq commited_strokes (pop redo_stack))
		(redraw t)))

(defun-bind commit (r s c)
	;commit a stroke to the canvas
	(push commited_strokes (list c (flatten r s))))

(defun-bind fpoly (col mode _)
	(canvas-set-color canvas col)
	(canvas-fpoly canvas 0 0 mode _))

(defun-bind redraw (&optional f)
	(when (or (> (defq now (time)) (+ then 10000)) f)
		(setq then now)
		(canvas-fill canvas argb_grey15)
		(each (lambda ((c s)) (fpoly c 1 s)) commited_strokes)
		(each (lambda ((r s)) (fpoly stroke_col 1 (flatten r s))) in_flight_strokes)
		(canvas-swap canvas)))

(defun-bind main ()
	(canvas-set-flags canvas 1)
	(redraw)
	(gui-add (apply view-change (cat (list window 256 256) (view-pref-size window))))
	(def image_scroll 'min_width min_width 'min_height min_height)
	(defq last_state 'u last_point nil)
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_close)
			nil)
		((= id event_min)
			;min button
			(apply view-change-dirty (cat (list window) (view-get-pos window)(view-pref-size window))))
		((= id event_max)
			;max button
			(def image_scroll 'min_width canvas_width 'min_height canvas_height)
			(apply view-change-dirty (cat (list window) (view-get-pos window)(view-pref-size window)))
			(def image_scroll 'min_width min_width 'min_height min_height))
		((<= event_black id event_magenta)
			;ink pot
			(setq stroke_col (elem (- id event_black) palette)))
		((<= event_radius1 id event_radius3)
			;stroke radius
			(setq stroke_radius (elem (- id event_radius1) radiuss)))
		((= id event_clear)
			;clear
			(snapshot)
			(clear commited_strokes)
			(redraw t))
		((= id event_undo)
			;undo
			(undo) t)
		((= id event_redo)
			;undo
			(redo) t)
		((= id (component-get-id canvas))
			;event for canvas
			(when (= (get-long msg ev_msg_type) ev_type_mouse)
				;mouse event in canvas
				(defq new_point (points (* (get-int msg ev_msg_mouse_rx) 1.0)
					(* (get-int msg ev_msg_mouse_ry) 1.0)))
				(cond
					((/= (get-int msg ev_msg_mouse_buttons) 0)
						;mouse button is down
						(case last_state
							(d	;was down last time, so extend last stroke ?
								(when (>= (vec-length (vec-sub new_point last_point)) (* 2 stroke_radius))
									(setq last_point new_point)
									(push (elem -2 (elem -2 in_flight_strokes)) (elem 0 new_point) (elem 1 new_point))))
							(u	;was up last time, so start new stroke
								(setq last_state 'd last_point new_point)
								(push in_flight_strokes (list stroke_radius new_point))))
						(redraw))
					(t	;mouse button is up
						(case last_state
							(d	;was down last time, so commit in flight strokes
								(setq last_state 'u)
								(snapshot)
								(each (lambda ((w s)) (commit w s stroke_col)) in_flight_strokes)
								(clear in_flight_strokes)
								(redraw t))
							(u	;was up last time, so we are hovering
								t))))) t)
		(t (view-event window msg))))
	;close window
	(view-hide window))
