;imports
(import 'apps/terminal/pipe.inc)

(defq prompt ">")
(defq enter_key (const (ascii-char 10)))

(defun cmd-prompt (msg enter)
	(if (eql enter nil)(setq enter_key ""))
	(terminal-output (cat msg enter_key prompt)))

(defq cmd_list '("cat" "dump" "echo" "lisp" "make" "null" "oops" "options"
	"shuffle" "sort" "tee" "tocpm" "unique")
(auto_cmd_list '("echo Welcome to ChrysaLisp..." "echo Please wipe your feet."))

(defun-bind terminal-output (_)
	(each (lambda (c)
		(setq c (code c))
		(if (= c 13) (setq c 10))
		(cond
			((= c 9)
				;print tab
				(pii-write-char 1 (const (ascii-code " ")))
				(pii-write-char 1 (const (ascii-code " ")))
				(pii-write-char 1 (const (ascii-code " ")))
				(pii-write-char 1 (const (ascii-code " "))))
			(t	;print char
				(pii-write-char 1 c)))) _))

(defun-bind terminal-input (c)
	(cond
		;send line ?
		((or (= c 10) (= c 13))
			;what state ?
			(cond
				(cmd
					;feed active pipe
					(pipe-write cmd (cat buffer enter_key)))
				(t	;start new pipe
					(cond
						((/= (length buffer) 0)
							;new pipe
							(catch (setq cmd (pipe-open buffer)) (progn (setq cmd nil) t))
							(unless cmd (cmd-prompt "Pipe Error !" t)))
						(t (cmd-prompt "" nil)))))
			(setq buffer ""))
		((= c 27)
			;esc
			(when cmd
				;feed active pipe, then EOF
				(when (/= (length buffer) 0)
					(pipe-write cmd buffer))
				(pipe-close cmd)
				(setq cmd nil buffer "")
				(cmd-prompt "" t) ))
		((and (= c 8) (/= (length buffer) 0))
			;backspace
			(setq buffer (slice 0 -2 buffer)))
		((<= 32 c 127)
			;buffer the char
			(setq buffer (cat buffer (char c))))))

;sign on msg
(cmd-prompt "ChrysaLisp Terminal 1.5" t)

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
			(cmd-prompt "" t))
		(t	;string from pipe
			(terminal-output data))))
