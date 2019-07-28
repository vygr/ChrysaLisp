;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_next)
	(byte 'win_prev))

(defq images '(apps/films/captive.flm apps/films/cradle.flm) index 0 id t)

(ui-tree window (create-window window_flag_close) nil
	(ui-element image_flow (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw))
		(ui-element _ (create-flow) ('flow_flags (logior flow_flag_right flow_flag_fillh)
				'color toolbar_col 'font (create-font "fonts/Entypo.otf" 32))
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_prev)
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_next))
		(ui-element frame (canvas-load (elem index images) load_flag_film))))

(gui-add (apply view-change (cat (list window 64 512)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) (elem index images))))))

(defun win-refresh (_)
	(view-sub frame)
	(setq index _ frame (canvas-load (elem index images) load_flag_film))
	(view-layout (view-add-back image_flow frame))
	(bind '(x y _ _) (view-get-bounds (window-set-title window (elem index images))))
	(bind '(w h) (view-pref-size window))
	(view-dirty-all (view-change (view-dirty window) x y w h)))

(while id
	(task-sleep 40000)
	(canvas-swap (canvas-next-frame frame))
	(while (defq msg (mail-trymail))
		(cond
			((eq (setq id (get-long msg ev_msg_target_id)) event_win_close)
				(setq id nil))
			((eq id event_win_next)
				(win-refresh (mod (inc index) (length images))))
			((eq id event_win_prev)
				(win-refresh (mod (add (dec index) (length images)) (length images))))
			(t (view-event window msg)))))
