;imports
(import 'sys/lisp.inc)

(defun-bind terminal-output (_)
	(each (lambda (c)
		(setq c (code c))
		(if (= c 13) (setq c 10))
		(cond
			;print char
			((= c 9)
				(pii-write-char 1 (const (ascii-code " ")))
				(pii-write-char 1 (const (ascii-code " ")))
				(pii-write-char 1 (const (ascii-code " ")))
				(pii-write-char 1 (const (ascii-code " "))))
			(t
				(pii-write-char 1 c)))) _))

(defun-bind terminal-input (c)
	(cond
		;send line ?
		((or (= c 10) (= c 13))
			;what state ?
			(cond
				(cmd
					;feed active pipe
					(pipe-write cmd (cat buffer (const (ascii-char 10)))))
				(t
					;start new pipe
					(cond
						((/= (length buffer) 0)
							;new pipe
							(catch (setq cmd (pipe buffer)) (progn (setq cmd nil) t))
							(unless cmd (terminal-output (const (cat "Pipe Error !" (ascii-char 10) ">")))))
						(t (terminal-output ">")))))
			(setq buffer ""))
		((= c 27)
			;esc
			(when cmd
				;feed active pipe, then EOF
				(when (/= (length buffer) 0)
					(pipe-write cmd buffer))
				(setq cmd nil buffer "")
				(terminal-output (const (cat (ascii-char 10) ">")))))
		((and (= c 8) (/= (length buffer) 0))
			;backspace
			(setq buffer (slice 0 -2 buffer)))
		((<= 32 c 127)
			;buffer the char
			(setq buffer (cat buffer (char c))))))

;sign on msg
(terminal-output (const (cat "ChrysaLisp Terminal 1.4" (ascii-char 10) ">")))

;create child and send args
(mail-send (list (task-mailbox)) (open-child "apps/terminal/tui_child.lisp" kn_call_open))

(defq cmd nil buffer "")
(while t
	(defq data t)
	(if cmd (setq data (pipe-read cmd t)))
	(cond
		((eql data t)
			;normal mailbox event
			(terminal-input (get-byte (mail-mymail) 0)))
		((eql data nil)
			;pipe is closed
			(setq cmd nil)
			(terminal-output (const (cat (ascii-char 10) ">"))))
		(t
			;string from pipe
			(terminal-output data))))
