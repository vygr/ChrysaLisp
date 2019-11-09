;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(defq id t index 0 xv 4 yv 0 i 512
	frames (map (lambda (_) (canvas-load (cat "apps/freeball/staoball_" (str _) ".cpm") load_flag_shared)) (range 1 12))
	sframes (map (lambda (_) (canvas-load (cat "apps/freeball/staoball_s_" (str _) ".cpm") load_flag_shared)) (range 1 12)))

(ui-tree view (create-view) nil
	(ui-element frame (elem 0 frames))
	(ui-element sframe (elem 0 sframes)))

(defq screen (penv (gui-add view)))

(while id
	(bind '(_ _ screen_width screen_height) (view-get-bounds screen))
	(defq index (% (inc index) (length frames))
		old_frame frame frame (elem index frames)
		old_sframe sframe sframe (elem index sframes))
	(bind '(ox oy w h) (view-get-bounds view))
	(bind '(_ _ fw fh) (view-get-bounds old_frame))
	(bind '(_ _ sw sh) (view-get-bounds old_sframe))
	(defq x (+ ox xv) y (+ oy yv) yv (inc yv))
	(if (> y (- screen_height fh)) (setq y (- screen_height fh) yv (neg (/ (* yv 8) 10))))
	(if (< x 0) (setq x 0 xv (abs xv)))
	(if (> x (- screen_width fw)) (setq x (- screen_width fw) xv (neg (abs xv))))
	(view-set-bounds frame 0 0 fw fh)
	(view-set-bounds sframe 8 32 sw sh)
	(view-sub old_sframe)
	(view-sub old_frame)
	(view-add-front (view-add-back view sframe) frame)
	(view-change-dirty view x y (+ 8 sw) (+ 32 sh))
	(setq id (/= 0 (setq i (dec i))))
	(while (mail-poll (array (task-mailbox)))
		(and (< (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) 0)
			(= (get-long msg ev_msg_type) ev_type_mouse)
			(setq id nil)))
	(task-sleep 40000))

(view-hide view)
