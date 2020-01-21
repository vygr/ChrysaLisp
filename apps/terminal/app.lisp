;imports
(import 'gui/lisp.inc)
(import 'apps/terminal/pipe.inc)
(import 'apps/terminal/input.inc)

(structure 'event 0
	(byte 'win_close))

(defq id t cmd nil vdu_width 60 vdu_height 40)

(ui-tree window (create-window (logior window_flag_close window_flag_status)) ('color 0xc0000000)
	(ui-element vdu (create-vdu) ('vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_green
		'font (create-font "fonts/Hack-Regular.ctf" 16))))

(gui-add (apply view-change (cat (list window 448 16)
	(view-pref-size (window-set-title (window-set-status
		(window-connect-close window event_win_close) "Ready") "Terminal")))))

;prompt
(defun-bind prompt () ">")

;override print for VDU output
(defun-bind print (_)
	(vdu-print vdu _))

;print line buf, truncate if needed
(defun-bind print-line (&optional flag)
	(defq p (if cmd "" (prompt)) g (- vdu_width (length p))
		l (if flag *line_buf* (line-with-cursor)))
	(when (> (length l) g)
		(defq is (max 0 (- *line_pos* g -1)) ie (min (+ is g) (length l)))
		(setq l (slice is ie l)))
	(print (cat (const (ascii-char vdu_clear_line)) p l)))

(defun-bind terminal-input (c)
	(line-input c)
	(cond
		((or (= c 10) (= c 13))
			;enter key
			(print-line t)
			(print (const (ascii-char 10)))
			(defq cmdline *line_buf*) (line-clear)
			(cond
				(cmd
					;feed active pipe
					(pipe-write cmd (cat cmdline (const (ascii-char 10)))))
				((/= (length cmdline) 0)
					;new pipe
					(catch (setq cmd (pipe-open cmdline)) (progn (setq cmd nil) t))
					(cond
						(cmd
							(view-dirty-all (window-set-status window "Busy")))
						(t
							(print (cat (const (cat "Pipe Error !" (ascii-char 10)))))
							(print-line))))
				(t	;empty cmdline
					(print-line))))
		((= c 27)
			;esc key
			(when cmd
				;feed active pipe, then EOF
				(when (/= (length *line_buf*) 0)
					(pipe-write cmd *line_buf*))
				(pipe-close cmd) (setq cmd nil) (line-clear)
				(view-dirty-all (window-set-status window "Ready"))
				(print-line)))
		(t	;some key
			(print-line))))

(print (cat (const (str "ChrysaLisp Terminal 1.5" (ascii-char 10))) (prompt) (line-with-cursor)))
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
			(print (cat (const (ascii-char 10)) (prompt) (line-with-cursor)))
			(view-dirty-all (window-set-status window "Ready")))
		(t	;string from pipe
			(print data))))

;close window and pipe
(view-hide window)
(if cmd (pipe-close cmd))
