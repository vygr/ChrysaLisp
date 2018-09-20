;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_next)
	(byte 'win_prev)
	(byte 'win_layout)
	(byte 'win_hvalue)
	(byte 'win_vvalue))

(defq images '(apps/images/frill.cpm apps/images/magicbox.cpm
	apps/images/captive.cpm apps/images/balls.cpm apps/images/banstand.cpm
	apps/images/bucky.cpm apps/images/circus.cpm apps/images/cyl_test.cpm
	apps/images/logo.cpm apps/images/mice.cpm apps/images/molecule.cpm
	apps/images/nippon3.cpm apps/images/piramid.cpm apps/images/rings.cpm
	apps/images/sharpend.cpm apps/images/stairs.cpm apps/images/temple.cpm
	apps/images/vermin.cpm) index 0 id t)

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw flow_flag_lasth)
			'color 0xff00ff00)
		(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_right flow_flag_fillh)
				'font (create-font "fonts/Entypo.otf" 32))
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_prev)
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_next))
		(ui-element image_flow (create-flow) ('flow_flags (bit-or flow_flag_left flow_flag_fillh flow_flag_lastw))
			(slider-connect-value (ui-element vslider (create-slider) nil) event_win_vvalue)
			(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_up flow_flag_fillw flow_flag_lasth))
				(slider-connect-value (ui-element hslider (create-slider) nil) event_win_hvalue)
				(ui-element image_view (create-view) ('min_width 256 'min_height 256)
					(ui-element image (create-view)))))))

(defun win-refresh (_)
	(view-sub image)
	(bind '(w h) (view-pref-size (setq index _ image (canvas-load (elem index images) 0))))
	(def hslider 'maximum 0 'portion 0 'value 0)
	(def vslider 'maximum 0 'portion 0 'value 0)
	(view-add-child image_view (view-set-bounds image 0 0 w h))
	(view-dirty-all (view-layout (window-set-title window (elem index images)))))

(defun win-layout ()
	(bind '(x y w h) (view-get-bounds image))
	(bind '(_ _ vw vh) (view-get-bounds image_view))
	(defq hval (get hslider 'value) vval (get vslider 'value)
		mho (max 0 (sub w vw)) mvo (max 0 (sub h vh)))
	(def hslider 'maximum mho 'portion vw 'value (min hval mho))
	(def vslider 'maximum mvo 'portion vh 'value (min vval mvo))
	(view-set-bounds image (max x (neg mho)) (max y (neg mvo)) w h)
	(view-dirty-all image_flow))

(window-connect-close window event_win_close)
(bind '(w h) (view-pref-size (win-refresh index)))
(gui-add (view-change window 64 64 w h))
(window-connect-layout window event_win_layout)
(win-layout)

(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_win_close)
			(setq id nil))
		((eq id event_win_next)
			(win-refresh (mod (inc index) (length images))))
		((eq id event_win_prev)
			(win-refresh (mod (add (dec index) (length images)) (length images))))
		((eq id event_win_layout)
			(win-layout))
		((eq id event_win_hvalue)
			(bind '(_ y w h) (view-get-bounds image))
			(view-set-bounds image (neg (get hslider 'value)) y w h)
			(view-dirty image_view))
		((eq id event_win_vvalue)
			(bind '(x _ w h) (view-get-bounds image))
			(view-set-bounds image x (neg (get vslider 'value)) w h)
			(view-dirty image_view))
		(t (view-event window msg))))
