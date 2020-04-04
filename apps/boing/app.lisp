;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close 'win_max 'win_min))

(defq id t index 0 xv 4 yv 0
	frames (map (lambda (_) (canvas-load (cat "apps/boing/taoball_" (str _) ".cpm") load_flag_shared)) (range 1 12))
	sframes (map (lambda (_) (canvas-load (cat "apps/boing/taoball_s_" (str _) ".cpm") load_flag_shared)) (range 1 12)))

(ui-window window ()
	(ui-flow _ ('flow_flags flow_down_fill)
		(ui-flow _ ('flow_flags flow_left_fill 'font (create-font "fonts/Entypo.ctf" 22) 'color *env_title_col*)
			(ui-buttons (0xea19 0xea1b 0xea1a) (const event_win_close))
			(ui-title _ ('text "Boing" 'font (create-font "fonts/OpenSans-Regular.ctf" 18))))
		(ui-element backdrop (create-backdrop) ('color argb_black 'ink_color argb_white)
			(ui-element frame (elem 0 frames))
			(ui-element sframe (elem 0 sframes)))))

(defun-bind main ()
	(gui-add (apply view-change (cat (list window 64 64) (view-pref-size window))))
	(while id
		(bind '(_ _ backdrop_width backdrop_height) (view-get-bounds backdrop))
		(defq index (% (inc index) (length frames))
			old_frame frame frame (elem index frames)
			old_sframe sframe sframe (elem index sframes))
		(bind '(x y w h) (view-get-bounds old_frame))
		(bind '(sx sy sw sh) (view-get-bounds old_sframe))
		(view-add-dirty (view-add-dirty backdrop sx sy sw sh) x y w h)
		(setq x (+ x xv) y (+ y yv) yv (inc yv))
		(if (> y (- backdrop_height h)) (setq y (- backdrop_height h) yv -22))
		(if (< x 0) (setq x 0 xv (abs xv)))
		(if (> x (- backdrop_width w)) (setq x (- backdrop_width w) xv (neg (abs xv))))
		(view-set-bounds frame x y w h)
		(view-set-bounds sframe (+ x 8) (+ y 64) sw sh)
		(view-sub old_sframe)
		(view-sub old_frame)
		(view-add-front (view-add-back backdrop sframe) frame)
		(view-dirty sframe)
		(view-dirty frame)
		(while (mail-poll (array (task-mailbox)))
			(cond
				((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
					(setq id nil))
				((= id event_win_min)
					;min button
					(bind '(x y _ _) (view-get-bounds window))
					(bind '(w h) (view-pref-size window))
					(view-change-dirty window x y w h))
				((= id event_win_max)
					;max button
					(bind '(x y _ _) (view-get-bounds window))
					(bind '(w h) (view-pref-size window))
					(view-change-dirty window x y (fmul w 1.5) (fmul h 1.5)))
				(t (view-event window msg))))
		(task-sleep 40000))
	(view-hide window))
