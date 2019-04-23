;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(ffi create-backdrop "apps/boing/backdrop/lisp_create" 0)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_min)
	(byte 'win_max))

(defq id t index 0 xv 4 yv 0
	frames (map (lambda (_) (canvas-load (cat "apps/boing/taoball_" (str _) ".cpm") load_flag_shared)) (range 1 12))
	sframes (map (lambda (_) (canvas-load (cat "apps/boing/taoball_s_" (str _) ".cpm") load_flag_shared)) (range 1 12)))

(ui-tree window (create-window (add window_flag_close window_flag_min window_flag_max)) nil
	(ui-element backdrop (create-backdrop) ('color argb_black 'ink_color argb_white)
		(ui-element sframe (elem 0 sframes))
		(ui-element frame (elem 0 frames))))

(window-set-title window "Boing")
(window-connect-close window event_win_close)
(window-connect-min window event_win_min)
(window-connect-max window event_win_max)
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 64 64 w h))

(while id
	(bind '(_ _ backdrop_width backdrop_height) (view-get-bounds backdrop))
	(task-sleep 40000)
	(defq index (mod (inc index) (length frames))
		old_frame frame frame (elem index frames)
		old_sframe sframe sframe (elem index sframes))
	(bind '(x y w h) (view-get-bounds old_frame))
	(bind '(sx sy sw sh) (view-get-bounds old_sframe))
	(view-add-dirty (view-add-dirty backdrop sx sy sw sh) x y w h)
	(setq x (add x xv) y (add y yv) yv (inc yv))
	(if (gt y (sub backdrop_height h)) (setq y (sub backdrop_height h) yv -22))
	(if (lt x 0) (setq x 0 xv (abs xv)))
	(if (gt x (sub backdrop_width w)) (setq x (sub backdrop_width w) xv (neg (abs xv))))
	(view-set-bounds frame x y w h)
	(view-set-bounds sframe (add x 8) (add y 64) sw sh)
	(view-sub old_sframe)
	(view-sub old_frame)
	(view-add-front (view-add-back backdrop sframe) frame)
	(view-dirty sframe)
	(view-dirty frame)
;	(debug "Pos = " x "," y)
	(while (defq msg (mail-trymail))
		(cond
			((eq (setq id (get-long msg ev_msg_target_id)) event_win_close)
				(setq id nil))
			((eq id event_win_min)
				;min button
				(bind '(x y _ _) (view-get-bounds (view-dirty window)))
				(bind '(w h) (view-pref-size window))
				(view-dirty-all (view-change window x y w h)))
			((eq id event_win_max)
				;max button
				(bind '(x y _ _) (view-get-bounds (view-dirty window)))
				(bind '(w h) (view-pref-size window))
				(view-dirty-all (view-change window x y (fmul w 1.5) (fmul h 1.5))))
			(t (view-event window msg)))))
