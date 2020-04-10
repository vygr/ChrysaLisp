;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

(structure 'event 0
	(byte 'win_close 'win_max 'win_min)
	(byte 'win_clear 'win_undo 'win_redo)
	(byte 'win_black 'win_red 'win_green 'win_blue 'win_cyan 'win_yellow 'win_magenta))

(defq canvas_width 640 canvas_height 480 min_width 320 min_height 240
	eps 0.25 min_len 4.0 stroke_width 3.0 stroke_col argb_black
	pallette (list argb_black argb_red argb_green argb_blue argb_cyan argb_yellow argb_magenta)
	commited_strokes (list) in_flight_strokes (list))

(ui-window window ()
	(ui-title-flow _ "Whiteboard" (0xea19 0xea1b 0xea1a) (const event_win_close))
	(ui-flow _ ('flow_flags (logior flow_flag_right flow_flag_fillh) 'color *env_toolbar_col* 'font *env_toolbar_font*)
		(ui-buttons (0xe94c 0xe9fe 0xe99d) (const event_win_clear))
		(each (lambda (col)
			(component-connect (ui-button __ ('color (const *env_toolbar2_col*) 'ink_color col
				'text (const (num-to-utf8 0xe95f)))) (+ _ event_win_black))) pallette))
	(ui-scroll image_scroll (logior scroll_flag_vertical scroll_flag_horizontal)
			('min_width canvas_width 'min_height canvas_height)
		(ui-canvas canvas canvas_width canvas_height 1)))

(defun-bind flatten (w s)
	;flatten a polyline to polygons
	(points-stroke-polylines w eps
		(const join_round) (const cap_round) (const cap_round)
		(list s) (list)))

(defun-bind commit (w s c)
	;commit a stroke to the canvas
	(push commited_strokes (list c (flatten w s))))

(defun-bind fpoly (col mode _)
	(canvas-set-color canvas col)
	(canvas-fpoly canvas 0 0 mode _))

(defun-bind redraw ()
	(canvas-fill canvas argb_white)
	(each (lambda ((c s))
		(fpoly c 1 s)) commited_strokes)
	(each (lambda ((w s))
		(fpoly stroke_col 1 (flatten w s))) in_flight_strokes)
	(canvas-swap canvas))

(defun-bind main ()
	(canvas-set-flags (canvas-fill canvas argb_white) 1)
	(gui-add (apply view-change (cat (list window 512 256) (view-pref-size window))))
	(def image_scroll 'min_width min_width 'min_height min_height)
	(defq last_state 'u last_point nil)
	(redraw)
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			nil)
		((= id event_win_min)
			;min button
			(apply view-change-dirty (cat (list window) (view-get-pos window)(view-pref-size window))))
		((= id event_win_max)
			;max button
			(def image_scroll 'min_width canvas_width 'min_height canvas_height)
			(apply view-change-dirty (cat (list window) (view-get-pos window)(view-pref-size window)))
			(def image_scroll 'min_width min_width 'min_height min_height))
		((<= event_win_black id event_win_magenta)
			;ink pot
			(setq stroke_col (elem (- id event_win_black) pallette)))
		((= id event_win_clear)
			;clear
			(clear commited_strokes)
			(redraw))
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
								(when (>= (vec-length (vec-sub new_point last_point)) min_len)
									(setq last_point new_point)
									(push (elem -2 (elem -2 in_flight_strokes)) (elem 0 new_point) (elem 1 new_point))
									(redraw)))
							(u	;was up last time, so start new stroke
								(setq last_state 'd last_point new_point)
								(push in_flight_strokes (list stroke_width new_point)))))
					(t	;mouse button is up
						(case last_state
							(d	;was down last time, so commit in flight strokes
								(setq last_state 'u)
								(each (lambda ((w s))
									(commit w s stroke_col)) in_flight_strokes)
								(clear in_flight_strokes)
								(redraw))
							(u	;was up last time, so we are hovering
								t))))) t)
		(t (view-event window msg))))
	;close window
	(view-hide window))
