;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/login/pupa.inc)

(structure 'event 0
	(byte 'win_button))

(ui-tree window (create-window 0) nil
	(each (lambda (path)
		(component-connect (ui-element _ (create-button) ('text path)) event_win_button)) *env_launcher_apps*))

(defun-bind app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun-bind main ()
	(each (lambda (_)
		(open-child (app-path _) kn_call_open)) *env_launcher_auto_apps*)
	(bind '(w h) (view-pref-size (window-set-title window "Launcher")))
	(gui-add (view-change window 16 16 (+ w 32) h))
	(while t
		(cond
			((= (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) event_win_button)
				(open-child (app-path (get (view-find-id window (get-long msg ev_msg_action_source_id)) 'text)) kn_call_open))
			(t (view-event window msg))))
	(view-hide window))
