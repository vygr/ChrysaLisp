;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_prev 'win_next))

(defq images '("apps/films/captive.flm" "apps/films/cradle.flm") index 0 id t)

(ui-window window ()
	(ui-flow window_flow ('flow_flags flow_down_fill)
		(ui-flow _ ('flow_flags flow_left_fill 'font (create-font "fonts/Entypo.ctf" 22) 'color *env_title_col*)
			(ui-buttons (0xea19) (const event_win_close))
			(ui-title window_title ('font (create-font "fonts/OpenSans-Regular.ctf" 18))))
		(ui-flow _ ('flow_flags (logior flow_flag_right flow_flag_fillh)
				'color *env_toolbar_col* 'font (create-font "fonts/Entypo.ctf" 32))
			(ui-buttons (0xe91d 0xe91e) (const event_win_prev)))
		(ui-element frame (canvas-load (elem index images) load_flag_film))))

(defun win-refresh (_)
	(view-sub frame)
	(setq index _ frame (canvas-load (elem index images) load_flag_film))
	(def window_title 'text (elem index images))
	(view-add-child window_flow frame)
	(bind '(x y _ _) (view-get-bounds window))
	(bind '(w h) (view-pref-size window))
	(view-change-dirty window x y w h))

(defun-bind main ()
	(gui-add (apply view-change (cat (list window 64 512) (view-pref-size (win-refresh index)))))
	(while id
		(task-sleep 40000)
		(canvas-swap (canvas-next-frame frame))
		(while (mail-poll (array (task-mailbox)))
			(cond
				((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
					(setq id nil))
				((= id event_win_next)
					(win-refresh (% (inc index) (length images))))
				((= id event_win_prev)
					(win-refresh (% (+ (dec index) (length images)) (length images))))
				(t (view-event window msg)))))
	(view-hide window))
