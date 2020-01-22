;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_login 'win_create))

(ui-tree window (create-window 0) nil
	(ui-element _ (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw flow_flag_lasth))
		(ui-element _ (create-flow) ('flow_flags (logior flow_flag_right flow_flag_fillh flow_flag_lastw))
			(ui-element _ (create-grid) ('grid_width 1 'grid_height 2)
				(ui-element _ (create-label) ('text "Username"))
				(ui-element _ (create-label) ('text "Password")))
			(ui-element _ (create-grid) ('grid_width 1 'grid_height 2 'color argb_white)
				(ui-element username (create-textfield) ('text (if (defq old (load "apps/login/current")) old "Guest")))
				(ui-element password (create-textfield) ('text "****************"))))
		(ui-element _ (create-grid) ('grid_width 2 'grid_height 1)
			(component-connect (ui-element _ (create-button) ('text "Login")) event_win_login)
			(component-connect (ui-element _ (create-button) ('text "Create")) event_win_create))))

(defun-bind position-window ()
	(bind '(w h) (view-pref-size window))
	(bind '(pw ph) (view-get-size (penv window)))
	(view-change-dirty window (/ (- pw w) 2) (/ (- ph h) 2) w h))

(defun-bind get-username ()
	(if (eql (defq user (get username 'text)) "") "Guest" user))

;add centered, wait a little for GUI to get going...
(task-sleep 10000)
(gui-add (window-set-title window "Login Manager"))
(position-window)

(defq id t)
(while id
	(cond
		((and (< (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) 0)
			(= (get-long msg ev_msg_type) ev_type_gui))
			;resized GUI
			(position-window))
		((= id event_win_login)
			;login button
			(when (/= (age (cat "apps/login/" (defq user (get-username)) "/pupa.inc")) 0)
				;login user
				(save user "apps/login/current")
				(open-child "apps/launcher/app.lisp" kn_call_open)
				(setq id nil)))
		((= id event_win_create)
			;create button
			(when (and (/= (age "apps/login/Guest/pupa.inc") 0)
					(= (age (cat (defq home (cat "apps/login/" (defq user (get-username)) "/")) "pupa.inc")) 0))
				;copy initial user files from Guest
				(save (load "apps/login/Guest/pupa.inc") (cat home "pupa.inc"))
				;login new user
				(save user "apps/login/current")
				(open-child "apps/launcher/app.lisp" kn_call_open)
				(setq id nil)))
		(t (view-event window msg))))

(view-hide window)
