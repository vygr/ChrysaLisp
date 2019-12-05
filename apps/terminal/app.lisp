;imports
(import 'gui/lisp.inc)
(import 'apps/terminal/pipe.inc)

(structure 'event 0
	(byte 'win_close))

(defq id t cmd nil buffer "" history (list) history_index 0 vdu_width 60 vdu_height 40)

(ui-tree window (create-window (logior window_flag_close window_flag_status)) ('color 0xc0000000)
	(ui-element vdu (create-vdu) ('vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_green
		'font (create-font "fonts/Hack-Regular.ttf" 16))))

(gui-add (apply view-change (cat (list window 0 0)
	(view-pref-size (window-set-title (window-set-status
		(window-connect-close window event_win_close) "Ready") "Terminal")))))
(vdu-print vdu (const (str "ChrysaLisp Terminal 1.5" (ascii-char 10) ">")))

(defun-bind terminal-output (c)
	(if (= c 13) (setq c 10))
	(cond
		;cursor up/down
		((<= 0x40000051 c 0x40000052)
			(vdu-print vdu (const (ascii-char 129))))
		;print char
		(t	(vdu-print vdu (char c)))))

(defun-bind terminal-input (c)
	;echo char to terminal unless backspace into prompt.
	(unless (and (= c 8) (= (length buffer) 0)) (terminal-output c))
	(cond
		;send line ?
		((or (= c 10) (= c 13))
			;what state ?
			(cond
				(cmd
					;feed active pipe
					(pipe-write cmd (cat buffer (const (ascii-char 10)))))
				(t	;start new pipe
					(cond
						((/= (length buffer) 0)
							;push new history entry if not same as last entry
							(and (> (length (push history buffer)) 1)
								(eql (elem -3 history) buffer)
								(pop history))
							(setq history_index (length history))
							;new pipe
							(catch (setq cmd (pipe-open buffer)) (progn (setq cmd nil) t))
							(if cmd
								(view-dirty-all (window-set-status window "Busy"))
								(vdu-print vdu (const (cat "Pipe Error !" (ascii-char 10) ">")))))
						(t (vdu-print vdu ">")))))
			(setq buffer ""))
		((= c 27)
			;esc
			(when cmd
				;feed active pipe, then EOF
				(when (/= (length buffer) 0)
					(pipe-write cmd buffer))
				(pipe-close cmd)
				(setq cmd nil buffer "")
				(vdu-print vdu (const (cat (ascii-char 10) ">")))
				(view-dirty-all (window-set-status window "Ready"))))
		((= c 0x40000052)
			;cursor up
			(unless cmd
				(vdu-print vdu ">")
				(setq buffer "" history_index (if (<= history_index 0) 0 (dec history_index)))
				(when (< history_index (length history))
					(setq buffer (elem history_index history))
					(vdu-print vdu buffer))))
		((= c 0x40000051)
			;cursor down
			(unless cmd
				(vdu-print vdu ">")
				(setq buffer "" history_index (min (inc history_index) (length history)))
				(when (< history_index (length history))
					(setq buffer (elem history_index history))
					(vdu-print vdu buffer))))
		((and (= c 8) (/= (length buffer) 0))
			;backspace
			(setq buffer (slice 0 -2 buffer)))
		((<= 32 c 127)
			;buffer the char
			(setq buffer (cat buffer (char c))))))

(while id
	(defq data t)
	(if cmd (setq data (pipe-read cmd)))
	(cond
		((eql data t)
			;normal mailbox event
			(cond
				((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
					(setq id nil))
				(t	(view-event window msg)
					(and (= (get-long msg ev_msg_type) ev_type_key)
						(> (get-int msg ev_msg_key_keycode) 0)
						(terminal-input (get-int msg ev_msg_key_key))))))
		((eql data nil)
			;pipe is closed
			(pipe-close cmd)
			(setq cmd nil)
			(vdu-print vdu (const (cat (ascii-char 10) ">")))
			(view-dirty-all (window-set-status window "Ready")))
		(t	;string from pipe
			(vdu-print vdu data))))

;close window and pipe
(view-hide window)
(if cmd (pipe-close cmd))
