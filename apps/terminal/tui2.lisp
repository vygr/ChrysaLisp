
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tui2 - Alternate ChrysaLisp TUI Terminal
; Adds internal switches and functions  in
; addition to just running commands user cmds
;
; To use, substitute tui2.lisp for tui.lisp
; in funcs.sh
;
; Internal switches:
;   See ijmptbl below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/pipe/pipe.inc")
(import "lib/logging/logservice.inc")
(import "lib/pathnode/pathnode.inc")
(import "lib/ipc/server_ipc.inc")

; Setup logging, service mailbox and timezones
(defq
  tlog      (anchor-logger "tui2")
  +banner  "ChrysaLisp Terminal-2 0.9 (RC-1)"
  tmserv    (server-ipc (mail-alloc-mbox))
  tzone     nil)

(mail-declare
  (. tmserv :service_mb)
  "TERMINAL_SERVICE"
  "Terminal Services 0.1")

(import "apps/terminal/tuiutils.lisp")

; Mailbox functions

(defun poll-mbox (ic &optional args)
  (defq qexist (mail-poll (list (. tmserv :service_mb))))
  (prtnl (str "Queue mail? " (if qexist "true" "false"))))

(defun service-mbox (ic &optional args)
  (not-impl ic args))

(defun send-mbox (ic &optional args)
  (not-impl ic args))

(defun client-mbox (ic &optional args)
  (not-impl ic args))

; Session functions

(defun session-sub (bfr)
  ; (session-sub string) -> string
  ; Scans string for substitution '@' from session vars
  (defq res (list))
  (each (lambda (_v)
          (if (eql (first _v) "@")
              (push res (gets-enval (rest _v)))
              (push res _v))) (split bfr " "))
  (join res " "))

(defun prompt ()
  ; (prompt) -> string
  (gets-enval "PROMPT"))

(defun _set-tz (tza)
  ; (_set-tz timezone-abbreviation) -> nil
  (defq
    tz   (timezone-lookup :abbreviation tza))
  (cond
    ; Do nothing, they are the same
    ((and tzone (eql (gets-enval "TZ") tza)))
     ; We have a hit
    (tz
      (log-info tlog (str "Setting timezone to " tza))
      (sets-envkvs! "TZ" (first (setq tzone tz))))
    ; Failed to find
    (t
      (prtnl (str tza " not found in timezones")))))

(defun set-session (ic &optional args)
  ; (set-session cmd [args]) -> map
  ; Sets the key (first arg) to remaining values of arg
  ; in the session map
  ; Special handling is done for "TZ"
  (when args
    (defq
      spa   (split args " ")
      _key  (first spa)
      _val  (rest spa))
    (cond
      ((eql _key "TZ")
       (_set-tz (second spa)))
      (t
        (sets-envkvs! _key (join _val " "))))))

(defun drop-session (ic &optional args)
  ; (drop-session cmd [args]) -> map
  ; Removes the key (first arg) from the session map
  (drop! +envcfg (first (split args " "))))

(defun print-session (ic &optional args)
  ; (print-session cmd [args]) -> map
  ; prints session values
  (prtnl "Session vars:")
  (each (lambda((_k _v)) (prtnl (str " " _k " -> " _v))) (entries +envcfg)))

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
  (run-cmd (gets-enval "LASTC")))

(defun switch-help (ic &optional args)
  (prtnl "")
  (prtnl +banner)
  (prtnl "")
  (prtnl "Switches:")
  (prtnl " -h   prints this help")
  (prtnl " -e   prints session variables")
  (prtnl " -e+  adds value to session (e.g. -e+ name Jane Doe")
  (prtnl " -e-  removes value from session (e.g. -e- name)")
  (prtnl " -c   re-executes last command")
  (prtnl "")
  (prtnl "Additioinal Commands:")
  (prtnl "")
  (prtnl " date - Displays current date and time")
  (prtnl "")
  (prtnl " ls - List directory contents or file. Usage:")
  (prtnl "  Synopsis: ls [-la] [file] [directory] ... ")
  (prtnl "    >ls     ; Simple listing of current directory")
  (prtnl "    >ls -l  ; Detailed listing of current directory")
  (prtnl "    >ls -a  ; Simple listing of all current directory")
  (prtnl "")
  (prtnl " cd - Changes directory. Usage:")
  (prtnl "    >cd newpath ; Change directory to newpath")
  (prtnl "")
  (prtnl " mkdir - Makes directories. Usage:")
  (prtnl "    >mkdir path ...      ; Fails if intermediate segment of path does not exist")
  (prtnl "    >mkdir -p path ...   ; Creates intermediates segments of path if needed")
  (prtnl "")
  (prtnl " pwd - Print current directory. Usage:")
  (prtnl "    >cd newpath")
  (prtnl "    >pwd  ; /Users/yourname/newpath")
  (prtnl "")
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
              "pwd"   current-directory ; Print current directory
              "cd"    change-directory  ; Change working directory
              "cp"    copy-file         ; Copy files
              "date"  disp-date         ; Prints date/time
              "mkdir" make-directory    ; Make a directory
              "ls"    list-files        ; File listing
              "rm"    delete-directory  ; Remove file or folder

              "-rc"   client-mbox       ; Register as client to service
              "-sc"   send-mbox         ; Send msg from terminal
              "-mp"   poll-mbox         ; Poll the terminal mailbox
              "-mr"   poll-mbox         ; Service terminal messages

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
      (prtnl (str "Internal command request " ic))
      (in ic (if (> (length sp) 1) (join (rest sp) " ") ""))
      (print (prompt)))
    (t
      ; New command pipe
      (sets-envkvs! "LASTC" bfr)
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
	((or (= c 9) (<= 32 c 127))
      ;buffer the char
      (setq buffer (cat buffer (char c))))))

(defun main ()
  ; Load path-nodes
  (defq continue t)
  ;sign on msg
  (prtnl +banner)
  (log-info tlog "Started Terminal 2")
  (while continue
    (catch
      (progn
        ; Set to working directory
        (change-dir (gets-enval "PWD"))
        ; (log-debug tlog (str "Setting _current_dir to " (. _current_dir :full_path)))
        ; Sets up timezone
        (if (nil? (defq tz (gets-enval "TZ")))
          (exports-keyvals! "TZ" (first (setq tzone (get :local_timezone))))
          (_set-tz tz))
        ; Prompt
        (print (prompt))
        ;create child and send args
        (mail-send (open-child "apps/terminal/tui_child.lisp" +kn_call_open) (task-mailbox))
        (defq cmd nil buffer "")
        (while t
          (defq data t)
          (if cmd (setq data (pipe-read cmd)))
          (cond
            ((eql data t)
              ;normal mailbox event
              (terminal-input (get-byte (mail-read (task-mailbox)) 0)))
            ((nil? data)
              ;pipe is closed
              (pipe-close cmd)
              (setq cmd nil)
              (print (prompt)))
            (t  ;string from pipe
              (print data)))))
      (progn
        (prtnl _)
        ; (log-debug tlog _)
        (print (prompt)))))
  )
