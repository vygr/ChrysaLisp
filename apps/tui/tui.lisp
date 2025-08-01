(import "lib/task/pipe.inc")

(defq tmbox (mail-mbox) +LF (ascii-char 10))
(mail-declare tmbox "Terminal" "Terminal Services 0.1")

;override print for TUI output
(redefun print (_)
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
			(:t ;print char
				(pii-write-char 1 c)))) _))

(defun prompt () ">")

(defun terminal-input (c)
	(cond
		;send line ?
		((or (= c 10) (= c 13))
			;what state ?
			(cond
				(cmd
					;feed active pipe
					(. cmd :write (cat buffer +LF)))
				(:t ;start new pipe
					(cond
						((/= (length buffer) 0)
							;new pipe
							(catch (setq cmd (Pipe buffer (list (task-mbox)))) (progn (setq cmd :nil) :t))
							(unless cmd (print (cat (const (cat "Pipe Error !" +LF)) (prompt)))))
						(:t (print (prompt))))))
			(setq buffer ""))
		((= c 27)
			;esc
			(when cmd
				;feed active pipe, then EOF
				(when (/= (length buffer) 0)
					(. cmd :write buffer))
				(. cmd :close)
				(setq cmd :nil buffer "")
				(print (cat +LF (prompt)))))
		((and (= c 8) (/= (length buffer) 0))
			;backspace
			(setq buffer (most buffer)))
		((or (= c 9) (<= 32 c 127))
			;buffer the char
			(setq buffer (cat buffer (char c))))))

(defun main ()
	;sign on msg
	(print (cat (const (cat "ChrysaLisp Terminal" +LF)) (prompt)))
	;create child and send args
	(mail-send (open-child "apps/tui/tui_child.lisp" +kn_call_open) (task-mbox))
	(defq cmd :nil buffer "")
	(while :t
		(defq data :t)
		(if cmd (setq data (. cmd :read)))
		(cond
			((eql data :t)
				;normal mailbox event
				(terminal-input (get-byte (mail-read (task-mbox)) 0)))
			((eql data :nil)
				;pipe is closed
				(. cmd :close)
				(setq cmd :nil)
				(print (const (cat +LF ">"))))
			(:t ;string from pipe
				(print data)))))
