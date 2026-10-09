(defq *env_user* "Guest")
(import "usr/Guest/env.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close login create))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Login Manager" () ())
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label _ (:text "Username:"))
		(ui-grid _ (:grid_width 1 :color +argb_white)
			(. (ui-textfield username (:hint_text "username" :min_width 192
				:clear_text (if (defq old (load "usr/current")) old "Guest")))
				:connect +event_login)))
	(ui-grid _ (:grid_height 1)
		(ui-buttons ("Login" "Create") +event_login)))

(defun position-window ()
	(bind '(w h) (. *window* :pref_size))
	(bind '(pw ph) (. (penv *window*) :get_size))
	(. *window* :change_dirty (/ (- pw w) 2) (/ (- ph h) 2) w h))

(defun get-username ()
	(if (eql (defq user (get :clear_text username)) "") "Guest" user))

(defun start-services ()
	(open-child "apps/system/wallpaper/app.lisp" +kn_call_pin)
	(open-child "service/clipboard/app.lisp" +kn_call_pin)
	(open-child "service/lock/app.lisp" +kn_call_run)
	;sound is played by the host of this node, the one with the GUI. Another
	;node of the machine may be on a host with no audio driver.
	(open-child "service/audio/app.lisp" +kn_call_pin)
	(open-child "service/net/app.lisp" +kn_call_run))

(defun ask ()
	;add centered
	(gui-add-front-rpc *window*)
	(position-window)
	(while (cond
		((and (< (defq id (getf (defq msg (mail-read (task-mbox))) +ev_msg_target_id)) 0)
			(= (getf msg +ev_msg_type) +ev_type_gui))
			;resized GUI
			(position-window))
		((= id +event_close)
			;app close
			:nil)
		((= id +event_login)
			;login button
			(cond
				((/= (age (cat "usr/" (defq user (get-username)) "/env.inc")) 0)
					;login user
					(save user "usr/current")
					(start-services)
					:nil)
				(:t :t)))
		((= id +event_create)
			;create button
			(cond
				((and (/= (age "usr/Guest/env.inc") 0)
					(= (age (cat (defq home (cat "usr/" (defq user (get-username)) "/")) "env.inc")) 0))
					;copy initial user files from Guest
					(save (load "usr/Guest/env.inc") (cat home "env.inc"))
					;login new user
					(save user "usr/current")
					(start-services)
					:nil)
				(:t :t)))
		((. *window* :event msg))))
	(gui-sub-rpc *window*))

(defun main ()
	;a session that was started as some user's, run.sh -u name, has no one
	;to ask, and the user the machine has, usr/current, is left as it is
	(if (get '*env_node_user*) (start-services) (ask)))
