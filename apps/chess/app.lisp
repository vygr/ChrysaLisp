;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(defq id t)

(ui-tree window (create-window window_flag_close) ('color argb_black)
	(ui-element vdu (create-vdu) ('vdu_width 39 'vdu_height 40 'ink_color argb_cyan
		'font (create-font "fonts/Hack-Regular.ttf" 16))))

(gui-add (apply view-change (cat (list window 512 128)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Chess")))))

;create child and send args
(mail-send (list vdu 5000000)
	(defq mbox (open-child "apps/chess/child.lisp" kn_call_open)))

(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			(setq id nil))
		(t (view-event window msg))))

;close child
(mail-send "" mbox)

(view-hide window)
