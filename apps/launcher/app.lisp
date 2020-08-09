;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'close 'button))

(ui-window window ()
	(ui-title-bar title "Launcher" (0xea19) (const event_close))
	;grid scales all buttons equally
	(ui-grid grid (:grid_width 1 :grid_height (length *env_launcher_apps*))
		(each (lambda (path)
			(component-connect (ui-button _ (:text path)) (const event_button))) *env_launcher_apps*)))

;first four are taken from wallpaper (screen sw sh and mouse x y)
(defun-bind launcher-position ((sw sh x y w h))
	(bind '(_ th) (view-pref-size title))
	(defq wx (- x (/ w 2)) wy (- y (/ h 2) (/ th 2)))
	(cond
		((eql *env_launcher_position* :top)
			(setq wy y))
		((eql *env_launcher_position* :bottom)
			(setq wy (- y h -1)))
		((eql *env_launcher_position* :left)
			(setq wx x))
		((eql *env_launcher_position* :right)
			(setq wx (- x w -1))))
	(list (max 0 (min wx (- sw w))) (max 0 (min wy (- sh h))) w h))

(defun-bind app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun-bind main ()
	;Ensure the launcher is completely on screen.
	(bind '(x y w h) (launcher-position (cat (mail-read (task-mailbox)) (view-pref-size window))))
	(gui-add (view-change window x y w h))
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_close)
			nil)
		((= id event_button)
			(open-child (app-path (get :text (view-find-id window (get-long msg ev_msg_action_source_id)))) kn_call_open)
			nil)
		(t (view-event window msg))))
	(view-hide window))
