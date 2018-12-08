;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_next)
	(byte 'win_prev))

(defq images '(apps/images/frill.cpm apps/images/magicbox.cpm
	apps/images/captive.cpm apps/images/balls.cpm apps/images/banstand.cpm
	apps/images/bucky.cpm apps/images/circus.cpm apps/images/cyl_test.cpm
	apps/images/logo.cpm apps/images/mice.cpm apps/images/molecule.cpm
	apps/images/nippon3.cpm apps/images/piramid.cpm apps/images/rings.cpm
	apps/images/sharpend.cpm apps/images/stairs.cpm apps/images/temple.cpm
	apps/images/vermin.cpm) index 0 image nil id t)

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw flow_flag_lasth)
			'color argb_green)
		(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_right flow_flag_fillh)
				'font (create-font "fonts/Entypo.otf" 32))
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_prev)
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_next))
		(ui-element image_scroll (create-scroll (bit-or scroll_flag_vertical scroll_flag_horizontal))
			('min_width 256 'min_height 256))))

(defun win-refresh (_)
	(bind '(w h) (view-pref-size (setq index _ image (canvas-load (elem index images) 0))))
	(view-layout (view-add-child image_scroll (view-set-bounds image 0 0 w h)))
	(view-dirty-all (view-layout (window-set-title window (elem index images)))))

(window-connect-close window event_win_close)
(bind '(w h) (view-pref-size (win-refresh index)))
(gui-add (view-change window 64 64 w h))

(while id
	(cond
		((eq (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((eq id event_win_next)
			(win-refresh (mod (inc index) (length images))))
		((eq id event_win_prev)
			(win-refresh (mod (add (dec index) (length images)) (length images))))
		(t (view-event window msg))))
