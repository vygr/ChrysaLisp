;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'close 'button))


(ui-window window ()
	(ui-title-bar _ "Launcher" (0xea19) (const event_close))
	;grid scales all buttons equally
	(ui-grid grid (:grid_width 1 :grid_height (length *env_launcher_apps*))
		(each (lambda (path)
			(component-connect (ui-button _ (:text path)) (const event_button))) *env_launcher_apps*)))

;first four are taken from clwm (screen sw sh and mouse x y)
(defun-bind launcher-position ((sw sh x y w h))
	;adjusted x by 16 so the cursor isn't on top of the title-bar :text.
	(defq w (+ w 32) x (max 0 (min (max (- x (+ 16 (/ w 2))) 0) (- sw w)))
			y (max 0 (min (max (- y 16) 0) (- sh h))))
	(list x y w h))

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
			(open-child (app-path (get :text (view-find-id window (get-long msg ev_msg_action_source_id)))) kn_call_open))
		(t (view-event window msg))))
	(view-hide window))
