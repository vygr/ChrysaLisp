;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

(ffi create-backdrop "apps/boing/backdrop/lisp_create" 0)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_min)
	(byte 'win_max))

(defq id t index 0 frames (map (lambda (_)
	(canvas-load (cat "apps/boing/taoball_" (str _) ".cpm") load_flag_shared)) (range 1 12)))

(ui-tree window (create-window (add window_flag_close window_flag_min window_flag_max)) nil
	(ui-element backdrop (create-backdrop) ('color argb_black)
		(ui-element frame (elem 0 frames))))

(window-set-title window "Boing")
(window-connect-close window event_win_close)
(window-connect-min window event_win_min)
(window-connect-max window event_win_max)
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 64 64 w h))

(bind '(w h) (view-pref-size frame))
(view-set-bounds frame 0 0 w h)
(defq xv 4 yv 0)

(while id
	(bind '(_ _ backdrop_width backdrop_height) (view-get-bounds backdrop))
	(task-sleep 40000)
	(defq index (mod (inc index) (length frames)) old_frame frame frame (elem index frames))
	(bind '(x y w h) (view-get-bounds old_frame))
	(view-add-dirty backdrop x y w h)
	(setq x (add x xv) y (add y yv) yv (inc yv))
	(if (gt y (sub backdrop_height h)) (setq y (sub backdrop_height h) yv -22))
	(if (lt x 0) (setq x 0 xv (abs xv)))
	(if (gt x (sub backdrop_width w)) (setq x (sub backdrop_width w) xv (neg (abs xv))))
	(view-set-bounds frame x y w h)
	(view-sub old_frame)
	(view-add-back backdrop frame)
	(view-dirty frame)
	(debug "Pos = " x "," y)
	(while (defq msg (mail-trymail))
		(cond
			((eq (setq id (read-long ev_msg_target_id msg)) event_win_close)
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
