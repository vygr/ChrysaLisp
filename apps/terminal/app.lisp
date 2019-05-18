;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(defq id t cmd nil buffer "" history (list) history_index 0 vdu_width 60 vdu_height 40)

(ui-tree window (create-window (bit-or window_flag_close window_flag_status)) ('color 0xc0000000)
	(ui-element vdu (create-vdu) ('vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_green
		'font (create-font "fonts/Hack-Regular.ttf" 16))))

(window-set-title window "Terminal")
(window-set-status window "Ready")
(window-connect-close window event_win_close)
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 0 0 w h))
(vdu-print vdu (cat "ChrysaLisp Terminal 1.4" (char 10) ">"))

(defun terminal-output (c)
	(if (eq c 13) (setq c 10))
	(cond
		;cursor up/down
		((le 0x40000051 c 0x40000052)
			(vdu-print vdu (char 129)))
		;print char
		(t
			(vdu-print vdu (char c)))))

(defun terminal-input (c)
	;echo char to terminal
	(terminal-output c)
	(cond
		;send line ?
		((or (eq c 10) (eq c 13))
			;what state ?
			(cond
				(cmd
					;feed active pipe
					(pipe-write cmd (cat buffer (char 10))))
				(t
					;start new pipe
					(cond
						((ne (length buffer) 0)
							;push new history entry if not same as last entry
							(and (gt (length (push history buffer)) 1)
								(eql (elem -3 history) buffer)
								(pop history))
							(setq history_index (length history))
							;new pipe
							(catch (setq cmd (pipe buffer)) (progn (setq cmd nil) t))
							(if cmd
								(view-dirty-all (window-set-status window "Busy"))
								(vdu-print vdu (cat "Pipe Error !" (char 10) ">"))))
						(t (vdu-print vdu ">")))))
			(setq buffer ""))
		((eq c 27)
			;esc
			(when cmd
				;feed active pipe, then EOF
				(when (ne (length buffer) 0)
					(pipe-write cmd buffer))
				(setq cmd nil buffer "")
				(vdu-print vdu (cat (char 10) ">"))
				(view-dirty-all (window-set-status window "Ready"))))
		((eq c 0x40000052)
			;cursor up
			(unless cmd
				(vdu-print vdu ">")
				(setq buffer "" history_index (if (le history_index 0) 0 (dec history_index)))
				(when (lt history_index (length history))
					(setq buffer (elem history_index history))
					(vdu-print vdu buffer))))
		((eq c 0x40000051)
			;cursor down
			(unless cmd
				(vdu-print vdu ">")
				(setq buffer "" history_index (min (inc history_index) (length history)))
				(when (lt history_index (length history))
					(setq buffer (elem history_index history))
					(vdu-print vdu buffer))))
		((and (eq c 8) (ne (length buffer) 0))
			;backspace
			(setq buffer (slice 0 -2 buffer)))
		((le 32 c 127)
			;buffer the char
			(setq buffer (cat buffer (char c))))))

(while id
	(defq data t)
	(if cmd (setq data (pipe-read cmd t)))
	(cond
		((eql data t)
			;normal mailbox event
			(cond
				((eq (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_close)
					(setq id nil))
				(t
					(view-event window msg)
					(and (eq (get-long msg ev_msg_type) ev_type_key)
						(gt (get-int msg ev_msg_key_keycode) 0)
						(terminal-input (get-int msg ev_msg_key_key))))))
		((eql data nil)
			;pipe is closed
			(setq cmd nil)
			(vdu-print vdu (cat (char 10) ">"))
			(view-dirty-all (window-set-status window "Ready")))
		(t
			;string from pipe
			(vdu-print vdu data))))
