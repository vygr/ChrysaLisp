
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tui2 - Experimental ChrysaLisp TUI Terminal
; Adds internal switches and functions  in
; addition to just running commands user cmds
;
; To use, substitute tui2.lisp for tui.lisp
; in funcs.sh
;
; Internal switches:
;   See ijmptbl below
;
; Planned internal commands (needs better support)
;   ls -> list files in current working directory
;   cd -> changes working directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "lib/pipe/pipe.inc")

;override print for TUI output
(defun print (_)
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
      (t  ;print char
        (pii-write-char 1 c)))) _))

(defun prtnl (_)
  (print (cat _ (ascii-char 0x0a))))

; Much of this should be in separate include

; Session variables
(defq
  session (xmap-kv
            :cwd    "../ChrysaLisp"
            :lsc    ""
            :prompt ">"))

(defun prompt ()
  ; (prompt) -> string
  (gets session :prompt))

(defun set-session (ic &optional args)
  ; (set-session cmd [args]) -> map
  ; Sets the key (first arg) to remaining values of arg
  ; in the session map
  (when args
    (defq spa (split args " "))
    (sets! session (sym (str ":" (first spa))) (join (rest spa) " "))))

(defun drop-session (ic &optional args)
  ; (drop-session cmd [args]) -> map
  ; Removes the key (first arg) from the session map
  (drop! session (sym (str ":" (first (split args " "))))))

(defun print-session (ic &optional args)
  ; (print-session cmd [args]) -> map
  ; prints session values
  (prtnl "Session vars:")
  (each (lambda((_k _v)) (prtnl (str " " _k " -> " _v))) (entries session)))

(defun session-sub (bfr)
  ; (session-sub string) -> string
  ; Scans string for substitution '@' from session vars
  (defq res (list))
  (each (lambda (_v)
          (if (eql (first _v) "@")
              (push res (gets session (sym (str ":" (rest _v)))))
              (push res _v))) (split bfr " "))
  (join res " "))

(defun run-cmd (bfr)
  ; (run-cmd buffer) -> result of command
  (catch (setq cmd (pipe-open (session-sub bfr))) (progn (setq cmd nil) t))
  (unless cmd
    (print (cat
             "Command '"
             bfr
             "' Error!"
             (ascii-char 10)
             (prompt)))))

(defun last-command (ic &optional args)
  ; (last-command internal args) -> result of command
  ; TBD indexed command arg (i.e. -c 2)
  (run-cmd (gets session :lsc)))

(defun switch-help (ic &optional args)
  (prtnl "")
  (prtnl "Switches:")
  (prtnl " -h   prints this help")
  (prtnl " -x   prints session variables")
  (prtnl " -x+  adds value to session (e.g. -x+ name Jane Doe")
  (prtnl " -x-  removes value from session (e.g. -x- name)")
  (prtnl " -c   re-executes last command")
  (prtnl "")
  (prtnl "Additioinal Commands:")
  (prtnl " ls   List directory content. Usage:")
  (prtnl "    >ls     ; Lists current working directory files")
  (prtnl "    >ls arg ; List directory content of arg path")
  (prtnl "")
  (prtnl " cd   Change directory (not implemented)")
  (prtnl "")
  (prtnl "Other:")
  (prtnl " @session-var   replaces @session-var with value")
  (prtnl "   example:")
  (prtnl "    >-x+ name Jane Doe")
  (prtnl "    >echo @name ; results in 'echo Jane Doe")
  (prtnl ""))

(defun list-files (ic &optional args)
  ; (list-files internal args) -> nil
  ; Lists files in either cwd or other in argument
  ; Flags include
  ; -? TBD
  (defq targ (if (= (length args) 0) (gets session :cwd) args))
  (each (#(if (not (or (eql %0 "4") (eql %0 "8")))
              (prtnl %0))) (split (pii-dirlist targ) ",")))

(defun fn (ic &optional args)
  (prtnl (str ic " -> not implemented")))

(defq
  ; Internal command dictionary
  ijmptbl (xmap-kv
              "-h"    switch-help   ; Help
              "ls"    list-files    ; File listing
              "cd"    fn            ; Change working directory
              "-x"    print-session ; Prints session values
              "-x+"   set-session   ; Add session value
              "-x-"   drop-session  ; Remove session value
              "-c"    last-command  ; Re-execute past command
              ))

(defun process-input (bfr)
  ; (process-input buffer) -> prompt
  ; Either process internal commands or pass
  ; to pipe to execute external commands
  (defq
    sp (split bfr " ")
    ic (first sp)
    in (gets ijmptbl ic))
  (cond
    (in
      ; Found internal command, execute and return
      (in ic (if (> (length sp) 1) (join (rest sp) " ") ""))
      (print (prompt)))
    (t
      ; New command pipe
      (sets! session :lsc bfr)
      (run-cmd bfr))))

(defun terminal-input (c)
  ; (terminal-input character)
  (cond
    ;send line ?
    ((or (= c 10) (= c 13))
      ;what state ?
      (cond
        (cmd
          ;feed active pipe
          (pipe-write cmd (cat buffer (ascii-char 10))))
        (t  ; otherwise
          (cond
            ((/= (length buffer) 0)
              ; Process input buffer content
              (process-input buffer))
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

(defun main ()
  ;sign on msg
  (print (cat (const (cat "ChrysaLisp Terminal-2 0.1 - experimental" (ascii-char 10))) (prompt)))
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
      (t  ;string from pipe
        (print data)))))
