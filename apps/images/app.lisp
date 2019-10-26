;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close 'win_next 'win_prev))

(defq images '(apps/images/frill.cpm apps/images/magicbox.cpm
	apps/images/captive.cpm apps/images/balls.cpm apps/images/banstand.cpm
	apps/images/bucky.cpm apps/images/circus.cpm apps/images/cyl_test.cpm
	apps/images/logo.cpm apps/images/mice.cpm apps/images/molecule.cpm
	apps/images/nippon3.cpm apps/images/piramid.cpm apps/images/rings.cpm
	apps/images/sharpend.cpm apps/images/stairs.cpm apps/images/temple.cpm
	apps/images/vermin.cpm) index 0 id t)

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw flow_flag_lasth)
			'color toolbar_col)
		(ui-element _ (create-flow) ('flow_flags (logior flow_flag_right flow_flag_fillh)
				'font (create-font "fonts/Entypo.otf" 32))
			(component-connect (ui-element _ (create-button) ('text "")) event_win_prev)
			(component-connect (ui-element _ (create-button) ('text "")) event_win_next))
		(ui-element image_scroll (create-scroll (logior scroll_flag_vertical scroll_flag_horizontal))
			('min_width 256 'min_height 256'color slider_col))))

(defun win-refresh (_)
	(view-layout (view-add-child image_scroll (canvas-load (elem (setq index _) images) 0)))
	(view-dirty-all (view-layout (window-set-title window (elem _ images)))))

(gui-add (apply view-change (cat (list window 64 64)
	(view-pref-size (window-connect-close (win-refresh index) event_win_close)))))

(while id
	(cond
		((= (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((= id event_win_next)
			(win-refresh (% (inc index) (length images))))
		((= id event_win_prev)
			(win-refresh (% (+ (dec index) (length images)) (length images))))
		(t (view-event window msg))))

(view-hide window)
