;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tuiutils - Terminal utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ; (current-directory internal args) -> nil
  ; Prints the current working directory
  (prtnl (. _current_dir :full_path))
  nil)

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

(defun copy-file (ic &optional args)
  ; (copy-file internal args) -> nil
  (bind '(sargs flags paths) (_split-args args))
  (cond
    ((> (length paths) 2)
     (prtnl (str "Usage: cp [] source_file target_file")))
    (t))
  (not-impl ic))

(defun disp-date (ic &optional args)
  ; (disp-date command args) -> nil
  (bind '(sargs flags paths) (_split-args args))
  (defq :local_timezone tzone)
  (prtnl (encode-date))
  nil)

(defun make-directory (ic &optional args)
  ; (make-directory internal args) -> nil
  ; Creates a directory
  (bind '(sargs flags paths) (_split-args args))
  (defq flgs  (_collapse_flags flags))
  (if (= (length paths) 0)
      (prtnl "mkdir [-p] path ...")
      (each
        (lambda (_el)
          (catch
            (if (gets flgs "p")
                (make-dir _el t)
                (make-dir _el))
            (prtnl (str "mkdir " _el ": threw " _)))) paths))
  nil)

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
    frmt  _pn-name-only
    fltr  _pn_short-filter
    mlst  :all_members)
  (defq
    flist (if (nempty? paths) paths (list ".")))

  (each
    (lambda (_el)
      (defq
        pname _el
        psplt (_path-tolist pname)
        fpath nil
        node  (node-for pname _pn_nofind-handler))
      (cond
        ((list? node)
         (bind '(segname pnode plistndx) node)
         (prtnl (str "stopped at " (. pnode :full_path)))
         (prtnl (str "stopped by " segname))
         (prtnl (str "can't get to " (join (slice plistndx -1 psplt) ""))))
        (t
          (prtnl (str "found " (. node :full_path)))))
          ) flist)
  ; (each (lambda (el)
  ;         (cond
  ;           ((eql el "l")
  ;            (setq frmt
  ;                  (lambda ((_fn _fm))
  ;                    (str "FL:" _fn))))
  ;           ((eql el "a")
  ;            (setq
  ;               mlst :all_members
  ;               fltr _pn-all-filter))
  ;           ((eql el "d")
  ;            (setq fltr _pn-dir-filter))
  ;           ((eql el "f")
  ;            (setq fltr _pn-file-filter))
  ;           (t t))) (entries flgs))

  ; (each prtnl (. _current_dir mlst frmt fltr))
  nil)

(defun delete-directory (ic &optional args)
  ; (del-directory internal args) -> nil
  ; Remove a directory
  ; TODO:
  ;   Recursive switch
  ;   Prompt
  ;   Silent
  (bind '(sargs flags paths) (_split-args args))
  (defq flgs  (_collapse_flags flags))
  (if (= (length paths) 0)
      (prtnl "rm [-r] [path | file] ...")
      (each
        (lambda (_el)
          (catch
            (if (gets flgs "r")
                ; (remove-dir _el t)
                ; (remove-dir _el)
                )
            (prtnl (str "rm " _el ": threw " _)))) paths))

  (not-impl ic)
  nil)
