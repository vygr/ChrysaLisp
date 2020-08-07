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

;first four are taken from clwm (screen sw sh and mouse x y)
(defun-bind launcher-position ((sw sh x y w h))
	(bind '(_ th) (view-pref-size title))
	; pad width by 20% and center x
	(defq w (+ w (/ w 5)) ox (/ w 2))
	(cond 
		((eql *env_launcher_position* "left")
			(setq ox 0))
		((eql *env_launcher_position* "right") (setq ox w))
		(t nil))
	(defq x (max (min (max (- x ox) 0) (- sw w)) 0)
					y (max (min (max (- y th) 0) (- sh h)) 0))
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