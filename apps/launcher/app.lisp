(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close button))

(ui-window *window* ()
	(ui-title-bar title "Launcher" (0xea19) +event_close)
	;grid scales all buttons equally
	(ui-grid grid (:grid_width 2 :grid_height (/ (inc (length *env_launcher_apps*)) 2))
		(each (lambda (p)
			(. (ui-button _ (:text p)) :connect +event_button)) *env_launcher_apps*)))

(defun app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun main ()
	;flush shared pixmaps
	(canvas-flush)
	;ensure the launcher is completely on screen
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (apply view-locate (push (list (/ (* w 100) 80) h) *env_launcher_position*)))
	(gui-add-front (. *window* :change x y w h))
	(while (cond
		((= (defq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)) +event_close)
			nil)
		((= id +event_button)
			(open-child (app-path (get :text (. *window* :find_id (getf msg +ev_msg_action_source_id)))) +kn_call_open)
			nil)
		(t (. *window* :event msg))))
	(gui-sub *window*))
