;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure '+event 0
	(byte 'close+ 'button+))

(ui-window window ()
	(ui-title-bar title "Launcher" (0xea19) +event_close+)
	;grid scales all buttons equally
	(ui-grid grid (:grid_width 1 :grid_height (length *env_launcher_apps*))
		(each (lambda (path)
			(component-connect (ui-button _ (:text path)) +event_button+)) *env_launcher_apps*)))

(defun-bind app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun-bind main ()
	;ensure the launcher is completely on screen
	(bind '(x y w h) (apply view-locate (push (view-pref-size window) *env_launcher_position*)))
	(gui-add (view-change window x y w h))
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +event_close+)
			nil)
		((= id +event_button+)
			(open-child (app-path (get :text (view-find-id window (get-long msg ev_msg_action_source_id)))) kn_call_open)
			nil)
		(t (view-event window msg))))
	(view-hide window))
