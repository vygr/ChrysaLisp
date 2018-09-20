;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

;single instance only
(unless (mail-enquire "DEBUG_SERVICE")
	(kernel-declare "DEBUG_SERVICE" (task-mailbox))

(structure 'debug_msg 0
	(long 'command)
	(long 'reply_id)
	(long 'tcb)
	(offset 'data))

(structure 'event 0
	(byte 'win_debug)
	(byte 'win_play)
	(byte 'win_stop)
	(byte 'win_step)
	(byte 'win_hvalue))

(ui-tree window (create-window window_flag_status) ('color 0xc0000000)
	(ui-element image_flow (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw))
		(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_right flow_flag_fillh)
				'color 0xff00ff00 'font (create-font "fonts/Entypo.otf" 32))
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_play)
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_stop)
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_step))
		(slider-connect-value (ui-element hslider (create-slider) ('color 0xffff0000)) event_win_hvalue)
		(ui-element vdu_flow (create-flow) ('flow_flags (bit-or flow_flag_fillw flow_flag_fillh)
				'text_color 0xffffff00 'font (create-font "fonts/Hack-Regular.ttf" 16) 'vdu_width 60 'vdu_height 40)
			(ui-element help (create-vdu)))))

(window-set-title window "Debug")
(window-set-status window "Ready")
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 0 0 w h))

(defq id t vdu_list (list help))
(vdu-print help (cat "ChrysaLisp Debug 0.1" (char 10)))

(while t
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_win_debug)
			(defq reply_id (read-long debug_msg_reply_id msg)
				tcb (read-long debug_msg_tcb msg)
				data (read-cstr debug_msg_data msg))
			(view-dirty (vdu-print help data))
			(mail-send "" reply_id))
		((eq id event_win_play))
		((eq id event_win_stop))
		((eq id event_win_step))
		(t (view-event window msg))))
)
