;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure '+event 0
	(byte 'close+ 'button+))

(ui-window mywindow ()
	(ui-title-bar title "Launcher" (0xea19) +event_close+)
	;grid scales all buttons equally
	(ui-grid grid (:grid_width 2 :grid_height (/ (inc (length *env_launcher_apps*)) 2))
		(each (lambda (path)
			(component-connect (ui-button _ (:text path)) +event_button+)) *env_launcher_apps*)))

(defun app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun main ()
	;ensure the launcher is completely on screen
	(bind '(w h) (view-pref-size mywindow))
	(bind '(x y w h) (apply view-locate (push (list (/ (* w 100) 80) h) *env_launcher_position*)))
	(gui-add (view-change mywindow x y w h))
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +event_close+)
			nil)
		((= id +event_button+)
			(open-child (app-path (get :text (view-find-id mywindow (get-long msg ev_msg_action_source_id)))) kn_call_open)
			nil)
		(t (view-event mywindow msg))))
	(view-hide mywindow))
