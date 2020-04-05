;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_button))

(ui-window window ()
	(ui-title _ ('text "Launcher"))
	(each (lambda (path)
		(component-connect (ui-button _ ('text path)) (const event_win_button))) *env_launcher_apps*))

(defun-bind app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun-bind main ()
	(each (lambda (_)
		(open-child (app-path _) kn_call_open)) *env_launcher_auto_apps*)
	(bind '(w h) (view-pref-size window))
	(gui-add (view-change window 16 16 (+ w 32) h))
	(while (cond
		((= (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) event_win_button)
			(open-child (app-path (get (view-find-id window (get-long msg ev_msg_action_source_id)) 'text)) kn_call_open))
		(t (view-event window msg))))
	(view-hide window))
