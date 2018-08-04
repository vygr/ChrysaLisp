;import settings
(run 'apps/sys.inc)
(run 'apps/ui.inc)

(structure 'event 0
	(byte 'win_close))

(defq id t index 0 canvas_width 640 canvas_height 480 frames (map (lambda (_)
	(canvas-load (cat "apps/boing/taoball_" (str _) ".cpm"))) (range 1 12)))

(ui-tree window (create-window window_flag_close) nil
	(ui-element canvas (create-canvas canvas_width canvas_height 1.0) nil
		(ui-element frame (elem 0 frames))))

(slot opaque canvas)
(slot fill canvas 0xff000000)
(defq x 0 y 0 w 32)
(while (lt x canvas_width)
	(slot set_fbox canvas 0xffffffff x 0 1 canvas_height)
	(setq x (add x w)))
(while (lt y canvas_height)
	(slot set_fbox canvas 0xffffffff 0 y canvas_width 1)
	(setq y (add y w)))

(slot set_title window "Boing")
(slot connect_close window event_win_close)
(bind '(w h) (slot pref_size window))
(slot change window 512 256 w h)
(slot gui_add window)

(bind '(w h) (slot pref_size frame))
(slot set_bounds frame 0 0 w h)
(defq xv 4 yv 0)

(while id
	(task-sleep 40000)
	(defq index (mod (inc index) (length frames)) old_frame frame frame (elem index frames))
	(bind '(x y w h) (slot get_bounds old_frame))
	(slot add_dirty canvas x y w h)
	(setq x (add x xv) y (add y yv) yv (inc yv))
	(if (gt y (sub canvas_height h)) (setq y (sub canvas_height h) yv -22))
	(if (lt x 0) (setq x 0 xv (abs xv)))
	(if (gt x (sub canvas_width w)) (setq x (sub canvas_width w) xv (neg (abs xv))))
	(slot set_bounds frame x y w h)
	(slot sub old_frame)
	(slot add_back canvas frame)
	(slot dirty frame)
	(while (defq msg (mail-trymail))
		(cond
			((eq (setq id (read-long ev_msg_target_id msg)) event_win_close)
				(setq id nil))
			(t (slot event window msg)))))
