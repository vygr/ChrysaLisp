
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;   ls -> list files
;   cd -> changes working directory
;   mkdir -> make a directory
;   rm -> removes files or directory
; TODO:
;   Load/reload persisted environmental variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;imports
(import "lib/pipe/pipe.inc")
(import "lib/date/date.inc")
(import "lib/logging/loganchor.inc")
(import "apps/terminal/tuiutils.lisp")

; Setup logging

(defq tlog (log-anchor "tui2"))

; Much of this should be in separate include

; Session variables
(defq
  tzone   nil
  session nil)

(defun prompt ()
  ; (prompt) -> string
  (gets session "PROMPT"))

(defun set-session (ic &optional args)
  ; (set-session cmd [args]) -> map
  ; Sets the key (first arg) to remaining values of arg
  ; in the session map
  (when args
    (defq spa (split args " "))
    (sets! session (first spa) (join (rest spa) " "))))

(defun drop-session (ic &optional args)
  ; (drop-session cmd [args]) -> map
  ; Removes the key (first arg) from the session map
  (drop! session (first (split args " "))))

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
              (push res (gets session (rest _v)))
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
  (run-cmd (gets session "LASTC")))

(defun switch-help (ic &optional args)
  (prtnl "")
  (prtnl "Switches:")
  (prtnl " -h   prints this help")
  (prtnl " -e   prints session variables")
  (prtnl " -e+  adds value to session (e.g. -e+ name Jane Doe")
  (prtnl " -e-  removes value from session (e.g. -e- name)")
  (prtnl " -c   re-executes last command")
  (prtnl "")
  (prtnl "Additioinal Commands:")
  (prtnl " ls   List directory content. Usage:")
  (prtnl "    >ls     ; Lists current working directory files")
  (prtnl "    >ls arg ; List directory content of arg path")
  (prtnl "")
  (prtnl " cd     Change directory (not implemented)")
  (prtnl " cp     Copies a file (not implemented)")
  (prtnl " mkdir  Makes a directory (not implemented)")
  (prtnl " mv     Moves a file (not implemented)")
  (prtnl " rm     Remove file or directory (not implemented)")
  (prtnl "")
  (prtnl "Other:")
  (prtnl " @session-var   replaces @session-var with value")
  (prtnl "   example:")
  (prtnl "    >-e+ name Jane Doe")
  (prtnl "    >echo @name ; results in 'echo Jane Doe")
  (prtnl ""))

(defq
  ; Internal command dictionary
  ijmptbl (xmap-kv
              "-h"    switch-help       ; Help
              "cd"    change-directory  ; Change working directory
              "cp"    copy-file         ; Copy files
              "date"  disp-date         ; Prints date/time
              "mkdir" make-directory    ; Make a directory
              "mv"    move-file
              "ls"    list-files        ; File listing
              "rm"    del-directory     ; Remove file or folder
              "-e"    print-session     ; Prints session values
              "-e+"   set-session       ; Add session value
              "-e-"   drop-session      ; Remove session value
              "-c"    last-command      ; Re-execute past command
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
      (sets! session "LASTC" bfr)
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
  ; Setup variables
  (setq tzone (get :local_timezone))
  ; TODO: Check and load configuration file
  (setq session (load-hostenv))
  (when (not (gets session "TZ"))
    (sets! session "TZ" (first tzone)))
  ;sign on msg
  (prtnl "ChrysaLisp Terminal-2 0.4 (experimental)")
  (print (prompt))
  (log-debug tlog "Started Terminal 2")
  ;create child and send args
  (mail-send
    (list (task-mailbox))
    (open-child "apps/terminal/tui_child.lisp" kn_call_open))
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
        (print (prompt)))
      (t  ;string from pipe
        (print data)))))
