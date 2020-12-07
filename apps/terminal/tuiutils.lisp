;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tuiutils - Terminal utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/pathnode/pathnode.inc")

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

(defun current-directory (ic &optional args)
  (prtnl (. _current_dir :full_path)))

(defun change-directory (ic &optional args)
  ; (change-directory internal args) -> nil
  ; Changes the working directory
  ; Implement by adding chdir and getcwd in main.c and
  ; calling from here
  (bind '(sargs flags paths) (_split-args args))
  (cond
    ((or (= (length paths) 0) (> (length paths) 1))
     (prtnl "Usage: cd pathname"))
    (t
      (defq path (first paths))
      (catch (sets-envkvs! "PWD" (. (change-dir path) :full_path))
        (prtnl (str "cd " path ": is not a valid path")))))
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
  ; (copy-file internal args) -> nil
  (bind '(sargs flags paths) (_split-args args))
  (cond
    ((> (length paths) 2)
     (prtnl (str "Usage: cp [] source_file target_file")))
    (t))
  (not-impl ic))

(defun move-file (ic &optional args)
  ; (move-file internal args) -> nil
  (bind '(sargs flags paths) (_split-args args))
  (not-impl ic))

(defun list-files (ic &optional args)
  ; (list-files internal args) -> nil
  ; Lists files in either cwd or other in argument
  ; emulates Linux/Darwin 'ls' command
  ; Flags include
  ; -l List in long format
  ; -d directories only
  ; -f files only
  ; -a All, include directory entries whose names begin with a dot (.)
  (bind '(sargs flags paths) (_split-args args))
  (defq
    flgs  (_collapse_flags flags)
    flist (if (nempty? paths) paths (list "."))
    frmt  _pn-name-only
    fltr  _pn_short-filter
    mlst  :all-members)
  (each (lambda (el)
          (cond
            ((eql el "l")
             (setq frmt
                   (lambda ((_fn _fm))
                     (str "FL:" _fn))))
            ((eql el "a")
             (setq
                mlst :all-members
                fltr _pn-all-filter))
            ((eql el "d")
             (setq fltr _pn-dir-filter))
            ((eql el "f")
             (setq fltr _pn-file-filter))
            (t t))) (entries flgs))

  (each prtnl (. _current_dir mlst frmt fltr))
  nil)

(defun disp-date (ic &optional args)
  ; (disp-date command args) -> nil
  (bind '(sargs flags paths) (_split-args args))
  (defq :local_timezone tzone)
  (prtnl (encode-date))
  nil)

(defun setup-pathing ()
  ; (setup-pathing) -> nil
  ; Sets current path to last working directory
  (change-dir (gets-enval "PWD")))