;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(defq images '("apps/images/wallpaper.cpm" "apps/images/chrysalisp.cpm")
	images_dims (map (lambda (_) (view-get-size (canvas-load _ load_flag_noswap))) images)
	wallpaper (create-view))

(defun refresh-wallpaper ()
	;pick nearest wallpaper to screen size
	(bind '(w h) (view-get-size screen))
	(defq index 0 err max_int)
	(each (lambda ((iw ih))
		(defq new_err (+ (* (- w iw) (- w iw)) (* (- h ih) (- h ih))))
		(when (< new_err err)
			(setq err new_err index _))) images_dims)
	(view-sub wallpaper)
	(setq wallpaper (canvas-load (elem index images) 0))
	(gui-add-back (view-change (view-dirty-all (view-set-flags wallpaper
		(const (+ view_flag_at_back view_flag_dirty_all))
		(const (+ view_flag_at_back view_flag_dirty_all)))) 0 0 w h)))

(defq screen (penv (gui-add-back wallpaper)))
(refresh-wallpaper)

(while t
	(when (and (< (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) 0)
			(= (get-long msg ev_msg_type) ev_type_gui))
		;resized GUI
		(refresh-wallpaper)))
