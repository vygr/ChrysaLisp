;imports
(import 'gui/lisp.inc)
(import 'apps/terminal/pipe.inc)
(import 'apps/terminal/input.inc)

(structure 'event 0
	(byte 'win_close))

(defq id t cmd nil vdu_width 60 vdu_height 40)

(defq prompt ">")
(defq enter_key (const (ascii-char 10)))

(defun cmd-prompt (msg enter)
	(if (eql enter nil)(setq enter_key ""))
	(vdu-print vdu (cat msg enter_key prompt)))

(defq cmd_list '("cat" "dump" "echo" "lisp" "make" "null" "oops" "options"
	"shuffle" "sort" "tee" "tocpm" "unique")
	auto_cmd_list '("echo Welcome to Chrysalisp..." "echo Please wipe your feet."))

(ui-tree window (create-window (logior window_flag_close window_flag_status)) ('color 0xc0000000)
	(ui-element vdu (create-vdu) ('vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_green
		'font (create-font "fonts/Hack-Regular.ttf" 16))))

(gui-add (apply view-change (cat (list window 0 0)
	(view-pref-size (window-set-title (window-set-status
		(window-connect-close window event_win_close) "Ready") "Terminal")))))
(cmd-prompt "ChrysaLisp Terminal 1.5" t)

(defun-bind terminal-output (c)
	(if (= c 13) (setq c 10))
	(vdu-print vdu (char c)))

(defun-bind terminal-input (c)
	(line-input c)
	(cond
		((or (= c 10) (= c 13))
			;enter key
			(vdu-print vdu enter_key)
			(cond
				(cmd
					;feed active pipe
					(pipe-write cmd (cat *line_buf* enter_key)))
				((/= (length *line_buf*) 0)
					;new pipe
					(catch (setq cmd (pipe-open *line_buf*)) (progn (setq cmd nil) t))
					(if cmd
						(view-dirty-all (window-set-status window "Busy"))
						(cmd-prompt "Pipe Error!" t))))
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
	(vdu-print vdu (cat (const (ascii-char 129)) (if cmd "" prompt) *line_buf*)))

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
			(cmd-prompt "" t)
			(view-dirty-all (window-set-status window "Ready")))
		(t	;string from pipe
			(vdu-print vdu data))))

;close window and pipe
(view-hide window)
(if cmd (pipe-close cmd))
