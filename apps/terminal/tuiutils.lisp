;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tuiutils - Terminal utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fauxfs/fauxfs.inc")

(defq
  current_path  nil
  +tenv-file+   "apps/terminal/.hostenv" )

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

(defun _split-args (args)
  ; (_split-args string) -> list
  ; Splits flags from arguments
  (defq sargs (split args " "))
  (reduce
    (lambda (acc el)
      (if (eql (first el) "-")
          (push (second acc) el)
          (push (last acc) el)) acc) sargs (list sargs (list) (list))))

(defun _collapse_flags (flags)
  ; (_collapse_flags flaglist) -> set
  ; Returns a set of flags from separate flags arguments
  (defq flgs (xset))
  (each (lambda (_) (reduce (#(progn (sets! %0 %1) %0)) (rest _) flgs)) flags)
  flgs)

(defun not-impl (ic &optional args)
  (prtnl (str ic " -> not implemented")))

(defun change-directory (ic &optional args)
  ; (change-directory internal args) -> nil
  ; Changes the working directory
  ; Implement by adding chdir and getcwd in main.c and
  ; calling from here
  (not-impl ic)
  nil)

(defun make-directory (ic &optional args)
  ; (make-directory internal args) -> nil
  ; Creates a directory
  ; Implement by using (file-stream path file_write_append)
  ; which is inefficient. Should have a make dir in the
  ; main kernel
  (bind '(sargs flags paths) (_split-args args))
  (not-impl ic)
  nil)

(defun del-directory (ic &optional args)
  ; (del-directory internal args) -> nil
  ; Remove a directory
  ; TODO:
  ;   Recursive switch
  ;   Prompt
  ;   Silent
  (bind '(sargs flags paths) (_split-args args))
  (not-impl ic)
  nil)

(defun copy-file (ic &optional args)
  (bind '(sargs flags paths) (_split-args args))
  (cond
    ((> (length paths) 2)
     (prtnl (str "Usage: cp [] source_file target_file")))
    (t))
  (not-impl ic))

(defun move-file (ic &optional args)
  (bind '(sargs flags paths) (_split-args args))
  (not-impl ic))

(defun list-files (ic &optional args)
  ; (list-files internal args) -> nil
  ; Lists files in either cwd or other in argument
  ; emulates Linux/Darwin 'ls' command
  ; Flags include
  ; -? TBD
  (bind '(sargs flags paths) (_split-args args))
  (defq
    flgs  (_collapse_flags flags)
    ; flist (file-lists
    ;         (if (empty? paths)
    ;             (list (gets session "PWD"))
    ;             paths))
    )
  (each prtnl (. current_path :all-members))
  ; (cond
  ;   ; Long listing for all (files and dirs)
  ;   ((and (gets flgs "l") (gets flgs "a"))
  ;    )
  ;   ; Long listing for files only
  ;   ((gets flgs "l")
  ;    )
  ;   ; Short listing for all
  ;   ((gets flgs "a")
  ;    )
  ;   ; Short listing for files only
  ;   (t
  ;     (each (lambda (_)
  ;             (prtnl (first _))
  ;             (each prtnl (files-only-short (first (rest _))))) flist)))
  nil)

(defun disp-date (ic &optional args)
  ; (disp-date command args) -> nil
  (bind '(sargs flags paths) (_split-args args))
  (prtnl (encode-date))
  nil)

(defun load-envmap (filename)
  ; (load-envmap filename) -> map
  ; Loads a map with key/value created from
  ; key=value
  ; etc.
  (defq
    xm (xmap)
    fs (file-stream filename))
  (while (defq ln (read-line fs))
    (defq splt (split ln "="))
    (sets! xm (first splt) (second splt)))
  xm)

(defun save-envmap (xm filename)
  ; (save-envmap map filename) -> map
  ; Dumps map to configuration file lines
  ; key=value
  ; etc.
  (defq
    fs (file-stream filename file_open_write))
  (each (lambda ((_k _v))
    (write-line fs (str _k "=" _v))) (entries xm))
  (stream-flush fs)
  xm)

(defun load-hostenv ()
  ; (load-hostenv target-hostenv) -> xmap
  ; Loads current host environment file
  ; If not found, bootstraps from ChrysaLisp/.hostenv
  ; and copies to ChrysaLisp/apps/terminal/.hostenv
  ; Returns xmap of key/values
  (defq
    +source-host-env+ ".hostenv"
    rm                nil)
  (when (= (age +source-host-env+) 0)
    (throw "Host environment file not found " +source-host-env+))
  (setq rm
    (cond
      ((= (age +tenv-file+) 0)
       (save-envmap
         (sets-pairs!
           (load-envmap +source-host-env+)
           "PROMPT" ">"
           "LASTC" nil
           "PATH" "cmd;apps")
         +tenv-file+))
      (t
        (load-envmap +tenv-file+))))
  (setq current_path (build-path (gets rm "PWD")))
  rm)