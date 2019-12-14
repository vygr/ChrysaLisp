;imports
(import 'gui/lisp.inc)
(import 'apps/terminal/pipe.inc)
(import 'apps/terminal/input.inc)

(structure 'event 0
	(byte 'win_close))

(defq id t cmd nil vdu_width 60 vdu_height 40)

(ui-tree window (create-window (logior window_flag_close window_flag_status)) ('color 0xc0000000)
	(ui-element vdu (create-vdu) ('vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_green
		'font (create-font "fonts/Hack-Regular.ttf" 16))))

(gui-add (apply view-change (cat (list window 448 16)
	(view-pref-size (window-set-title (window-set-status
		(window-connect-close window event_win_close) "Ready") "Terminal")))))
(vdu-print vdu (const (str "ChrysaLisp Terminal 1.5" (ascii-char 10) ">")))

(defun-bind terminal-output (c)
	(if (= c 13) (setq c 10))
	(vdu-print vdu (char c)))

(defun-bind terminal-input (c)
	(line-input c)
	(cond
		((or (= c 10) (= c 13))
			;enter key
			(vdu-print vdu (const (ascii-char 10)))
			(cond
				(cmd
					;feed active pipe
					(pipe-write cmd (cat *line_buf* (const (ascii-char 10)))))
				((/= (length *line_buf*) 0)
					;new pipe
					(catch (setq cmd (pipe-open *line_buf*)) (progn (setq cmd nil) t))
					(if cmd
						(view-dirty-all (window-set-status window "Busy"))
						(vdu-print vdu (const (cat "Pipe Error !" (ascii-char 10) ">"))))))
			(setq *line_buf* ""))
		((= c 27)
			;esc key
			(when cmd
				;feed active pipe, then EOF
				(when (/= (length *line_buf*) 0)
					(pipe-write cmd *line_buf*))
				(pipe-close cmd)
				(setq cmd nil *line_buf* "")
				(vdu-print vdu (const (ascii-char 10)))
				(view-dirty-all (window-set-status window "Ready")))))
	(vdu-print vdu (cat (const (ascii-char 129)) (if cmd "" ">") *line_buf*)))

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
