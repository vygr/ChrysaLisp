(import "usr/env.inc")
(import "lib/task/pipe.inc")
(import "lib/files/files.inc")
(import "apps/system/terminal/state.inc")

(defq tmbox (mail-mbox)
	+LF (ascii-char 10)
	+CR (ascii-char 13)
	+ESC (ascii-char 27)
	+CSI (cat +ESC "[")
	+state_filename "terminal.tre")

;override print for TUI output
(redefun print (&rest args)
	(if (empty? args) (setq args (list +CR +LF)))
	(each (lambda (a)
		(each (lambda (c)
			(setq c (code c))
			(cond
				((= c 9)
					;print tab
					(pii-write-char 1 32) (pii-write-char 1 32)
					(pii-write-char 1 32) (pii-write-char 1 32))
				(:t ;print char
					(pii-write-char 1 c))))
			(str a)))
		args))

(defun prompt () ">")

(defun redraw-line ()
	;CR, Prompt, Buffer, Clear-To-End
	(print +CR (prompt) buffer +CSI "K")
	;Move cursor to visual position: CR, Prompt, Right-N
	(print +CR (prompt))
	(if (> cursor 0) (print +CSI (str cursor) "C")))

(defun terminal-input (c)
	(if (and (= c 10) (= last_input 13))
		(setq c 0) ; Ignore LF after CR
		(setq last_input c))
	(if (> c 0)
	(case esc_state
		(0 ; Normal state
			(cond
				((= c 27) ; ESC
					(setq esc_state 1))
				((or (= c 10) (= c 13)) ; Enter
					(print +CR +LF) ; Newline
					(cond
						(cmd ; Feed active pipe
							(. cmd :write (cat buffer +LF)))
						(:t ; New command
							(when (> (length buffer) 0)
								; Start pipe
								(catch (setq cmd (Pipe buffer (list (task-mbox))))
									(progn (setq cmd :nil) :t))
								(unless cmd (print "Pipe Error !" +LF))
								(when cmd
									; Add to history if unique at end
									(when (or (empty? *history*) (nql (last *history*) buffer))
										(push *history* buffer))
									(state-save)
									(setq *history_idx* (length *history*))))))
					(if (not cmd) (print (prompt)))
					(setq buffer "" cursor 0))
				((= c 127) ; Backspace (often 127 on TUI)
					(when (> cursor 0)
						(setq buffer (erase buffer (dec cursor) cursor))
						(-- cursor)
						(redraw-line)))
				((= c 8) ; Ctrl-H Backspace
					(when (> cursor 0)
						(setq buffer (erase buffer (dec cursor) cursor))
						(-- cursor)
						(redraw-line)))
				((= c 4) ; Ctrl-D (EOF/Delete)
					(if (= (length buffer) 0)
						(if cmd
							(progn ; EOF to pipe
								(. cmd :close)
								(setq cmd :nil buffer "")
								(print +CR +LF (prompt)))
							(progn ; Exit shell
								(print +CR +LF "Exiting..." +CR +LF)
								(gui-quit-rpc)))
						(progn ; Delete char at cursor
							(when (< cursor (length buffer))
								(setq buffer (erase buffer cursor (inc cursor)))
								(redraw-line)))))
				((or (= c 9) (<= 32 c 126)) ; Printable or Tab
					(setq buffer (insert buffer cursor (char c)))
					(++ cursor)
					(redraw-line))))
		(1 ; Expecting [
			(if (= c 91)
				(setq esc_state 2)
				(setq esc_state 0))) ; Malformed, reset
		(2 ; Command char
			(cond
				((= c 65) ; Up Arrow
					(when (> *history_idx* 0)
						(-- *history_idx*)
						(setq buffer (elem-get *history* *history_idx*)
							  cursor (length buffer))
						(redraw-line))
					(setq esc_state 0))
				((= c 66) ; Down Arrow
					(cond
						((< *history_idx* (dec (length *history*)))
							(++ *history_idx*)
							(setq buffer (elem-get *history* *history_idx*)))
						(:t (setq buffer "" *history_idx* (length *history*))))
					(setq cursor (length buffer))
					(redraw-line)
					(setq esc_state 0))
				((= c 67) ; Right Arrow
					(if (< cursor (length buffer)) (++ cursor))
					(redraw-line)
					(setq esc_state 0))
				((= c 68) ; Left Arrow
					(if (> cursor 0) (-- cursor))
					(redraw-line)
					(setq esc_state 0))
				((= c 72) ; Home (Standard)
					(setq cursor 0) (redraw-line) (setq esc_state 0))
				((= c 70) ; End (Standard)
					(setq cursor (length buffer)) (redraw-line) (setq esc_state 0))
				((= c 49) ; Home/End/Del? (1~)
					(setq esc_state 3)) ; Wait for ~
				((= c 51) ; Del (3~)
					(setq esc_state 4)) ; Wait for ~
				((= c 52) ; End (4~)
					(setq esc_state 5)) ; Wait for ~
				(:t (setq esc_state 0))))
		(3 ; Home (1~)
			(when (= c 126) (setq cursor 0) (redraw-line))
			(setq esc_state 0))
		(4 ; Del (3~)
			(when (= c 126)
				(when (< cursor (length buffer))
					(setq buffer (erase buffer cursor (inc cursor)))
					(redraw-line)))
			(setq esc_state 0))
		(5 ; End (4~)
			(when (= c 126) (setq cursor (length buffer)) (redraw-line))
			(setq esc_state 0)))))

(defun main ()
	;sign on msg
	(mail-declare tmbox "Terminal" "Terminal Services 0.2")
	(print "ChrysaLisp Terminal" +LF (prompt))
	;create child and send args
	(mail-send (open-child "apps/tui/tui_child.lisp" +kn_call_open) (task-mbox))
	(defq cmd :nil buffer "" cursor 0 esc_state 0 last_input 0
		*meta_map* :nil *history_idx* (state-load))
	(bind '(*history*) (gather *meta_map* :history))
	(while :t
		(defq data :t)
		(if cmd (setq data (. cmd :read)))
		(cond
			((eql data :t)
				;normal mailbox event
				(terminal-input (code (mail-read (task-mbox)))))
			((eql data :nil)
				;pipe is closed
				(. cmd :close)
				(setq cmd :nil)
				(print +CR +LF (prompt)))
			(:t ;string from pipe
				(print data)))))