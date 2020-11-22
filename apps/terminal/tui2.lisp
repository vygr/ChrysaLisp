
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
(import "sys/lisp.inc")
(import "lib/pipe/pipe.inc")
(import "lib/date/date.inc")

(import "lib/logging/loganchor.inc")

; Setup logging

(defq tlog (log-anchor "tui2"))

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
  tzone   nil
  session (emap-kv
            :cwd    "../ChrysaLisp"     ; Current working directory
            :cpth   "cmd"               ; Search path for commands
            :lsc    ""                  ; Last command slot
            :tz     nil                 ; Timezone
            :prompt ">"))               ; Prompt specialization

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
  (prtnl " mkdir  Makes a directory (not implemented)")
  (prtnl " rm     Remove file or directory (not implemented)")
  (prtnl "")
  (prtnl "Other:")
  (prtnl " @session-var   replaces @session-var with value")
  (prtnl "   example:")
  (prtnl "    >-e+ name Jane Doe")
  (prtnl "    >echo @name ; results in 'echo Jane Doe")
  (prtnl ""))

(defun grab-flags (coll)
  (filter (#(eql (first %0) "-")) coll))

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

(defun change-directory (ic &optional args)
  ; (change-directory internal args) -> map
  ; Changes the working directory
  ; Implement by adding chdir and getcwd in main.c and
  ; calling from here
  ; (if (> (age args) 0)
  ;     (sets! session :cwd args)
  ;     (prtnl (str "Directory '" args "' does not exist")))
  (fn ic))

(defun make-directory (ic &optional args)
  ; (make-directory internal args) -> ?
  ; Creates a directory
  ; Implement by exposing rmkdir in main.c and
  ; calling from here
  (defq
    aprs (split args " ")
    pth  (pop aprs)
    flgs (grab-flags aprs))
  (prtnl "With flags:")
  (each prtnl flgs)
  (prtnl (str "Creating: " pth)))

(defun del-directory (ic &optional args)
  ; (del-directory internal args) -> ?
  ; Remove a directory
  ; TODO:
  ;   Recursive switch
  ;   Prompt
  ;   Silent
  (fn ic))

(defq
  ; Internal command dictionary
  ijmptbl (xmap-kv
              "-h"    switch-help       ; Help
              "ls"    list-files        ; File listing
              "cd"    change-directory  ; Change working directory
              "mkdir" make-directory    ; Make a directory
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
  ; Setup variables
  (setq tzone (get :local_timezone))
  (sets! session :tz (first tzone))
  ; TODO: Check and load configuration file
  ;sign on msg
  (prtnl "ChrysaLisp Terminal-2 0.3 (experimental)")
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
