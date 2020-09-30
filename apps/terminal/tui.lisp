;imports
(import "lib/pipe/pipe.inc")

;override print for TUI output
(defun-bind print (_)
	(each (lambda (c)
		(setq c (code c))
		(if (= c 13) (setq c 10))
		(cond
			((= c 9)
				;print tab
				(pii-write-char 1 (ascii-code " "))
				(pii-write-char 1 (ascii-code " "))
				(pii-write-char 1 (ascii-code " "))
				(pii-write-char 1 (ascii-code " ")))
			(t	;print char
				(pii-write-char 1 c)))) _))

(defun-bind prompt () ">")

(defun-bind terminal-input (c)
	(cond
		;send line ?
		((or (= c 10) (= c 13))
			;what state ?
			(cond
				(cmd
					;feed active pipe
					(pipe-write cmd (cat buffer (ascii-char 10))))
				(t	;start new pipe
					(cond
						((/= (length buffer) 0)
							;new pipe
							(catch (setq cmd (pipe-open buffer)) (progn (setq cmd nil) t))
							(unless cmd (print (cat (const (cat "Pipe Error !" (ascii-char 10))) (prompt)))))
						(t (print (prompt))))))
			(setq buffer ""))
		((= c 27)
			;esc
			(when cmd
				;feed active pipe, then EOF
				(when (/= (length buffer) 0)
					(pipe-write cmd buffer))
				(pipe-close cmd)
				(setq cmd nil buffer "")
				(print (cat (ascii-char 10) (prompt)))))
		((and (= c 8) (/= (length buffer) 0))
			;backspace
			(setq buffer (slice 0 -2 buffer)))
		((<= 32 c 127)
			;buffer the char
			(setq buffer (cat buffer (char c))))))

(defun-bind main ()
	;sign on msg
	(print (cat (const (cat "ChrysaLisp Terminal 1.5" (ascii-char 10))) (prompt)))
	;create child and send args
	(mail-send (list (task-mailbox)) (open-child "apps/terminal/tui_child.lisp" kn_call_open))
	(defq cmd nil buffer "")
	(while t
		(defq data t)
		(if cmd (setq data (pipe-read cmd)))
		(cond
			((eql data t)
				;normal mailbox event
				(terminal-input (get-byte (mail-read (task-mailbox)) 0)))
			((eql data nil)
				;pipe is closed
				(pipe-close cmd)
				(setq cmd nil)
				(print (const (cat (ascii-char 10) ">"))))
			(t	;string from pipe
				(print data)))))
