;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/terminal/pipe.inc)
(import 'apps/terminal/input.inc)
(import 'apps/login/pupa.inc)

(structure 'event 0
	(byte 'win_close 'win_min 'win_max))

(defq id t cmd nil vdu_width 60 vdu_height 40 text_buf (list ""))

(ui-tree window (create-window (+ window_flag_close window_flag_min window_flag_max window_flag_status)) ('color 0xc0000000)
	(ui-element vdu (create-vdu) ('vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_green
		'font (create-font "fonts/Hack-Regular.ctf" 16))))

(gui-add (apply view-change (cat (list window 448 16)
	(view-pref-size (window-set-title (window-set-status (window-connect-close (window-connect-min
		(window-connect-max window event_win_max) event_win_min) event_win_close) "Ready") "Terminal")))))

(defun-bind vdu-print (vdu buf s)
	(each (lambda (c)
		(cond
			((eql c (const (ascii-char 10)))
				;line feed and truncate
				(push buf "")
				(if (> (length buf) vdu_height)
					(setq buf (slice (dec (neg vdu_height)) -1 buf))))
			((eql c (const (ascii-char 126)))
				;clear line
				(elem-set -2 buf ""))
			(t	;char
				(elem-set -2 buf (cat (elem -2 buf) c))))) s)
	;set cursor and offset
	(defq cx (if cmd *line_pos* (+ (length *env_terminal_prompt*) *line_pos*))
		cy (dec (length buf)) ox 0 oy 0)
	(cond
		((< cx ox)
			(setq ox cx))
		((>= cx (+ ox vdu_width))
			(setq ox (- cx vdu_width -1))))
	(cond
		((< cy oy)
			(setq oy cy))
		((>= cy (+ oy vdu_height))
			(setq oy (- cy vdu_height -1))))
	(vdu-load vdu buf ox oy cx cy) buf)

;override print for VDU output
(defun-bind print (_)
	(setq text_buf (vdu-print vdu text_buf _)))

(defun-bind print-edit-line ()
	(print (cat (const (ascii-char 126)) (if cmd "" *env_terminal_prompt*) *line_buf*)))

(defun-bind terminal-input (c)
	(line-input c)
	(cond
		((or (= c 10) (= c 13))
			;enter key
			(print-edit-line)
			(print (const (ascii-char 10)))
			(defq cmdline *line_buf*)
			(line-clear)
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
							(print-edit-line))))
				(t	;empty cmdline
					(print-edit-line))))
		((= c 27)
			;esc key
			(when cmd
				;feed active pipe, then EOF
				(when (/= (length *line_buf*) 0)
					(pipe-write cmd *line_buf*))
				(pipe-close cmd) (setq cmd nil) (line-clear)
				(view-dirty-all (window-set-status window "Ready"))
				(print-edit-line)))
		(t	;some key
			(print-edit-line))))

(defun-bind vdu-resize (w h)
	(setq vdu_width w vdu_height h)
	(set vdu 'vdu_width vdu_width 'vdu_height vdu_height)
	(bind '(x y _ _) (view-get-bounds window))
	(bind '(w h) (view-pref-size window))
	(view-change-dirty window x y w h)
	(print-edit-line))

(print (str "ChrysaLisp Terminal 1.6" (ascii-char 10)))
(print-edit-line)
(while id
	(defq data t)
	(if cmd (setq data (pipe-read cmd)))
	(cond
		((eql data t)
			;normal mailbox event
			(cond
				((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
					(setq id nil))
				((= id event_win_min)
					;min button
					(vdu-resize 60 40))
				((= id event_win_max)
					;max button
					(vdu-resize 120 40))
				(t	(view-event window msg)
					(and (= (get-long msg ev_msg_type) ev_type_key)
						(> (get-int msg ev_msg_key_keycode) 0)
						(terminal-input (get-int msg ev_msg_key_key))))))
		((eql data nil)
			;pipe is closed
			(pipe-close cmd)
			(setq cmd nil)
			(print (cat (const (ascii-char 10)) *env_terminal_prompt* *line_buf*))
			(view-dirty-all (window-set-status window "Ready")))
		(t	;string from pipe
			(print data))))

;close window and pipe
(view-hide window)
(if cmd (pipe-close cmd))
