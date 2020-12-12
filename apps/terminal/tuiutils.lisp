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

(defun _print-long-entry (name stat)
  ; (_print-long-engry name stat) -> nil
  ; Prints details about file or directory in name
  (defq :local_timezone tzone)
  (bind '(s m h dy mo yr wk) (date (first stat)))
  (prtnl
    (str
      (mode-str stat) " "
      (fsize-str (second stat)) " "
      (month-of-the-year mo) " "
      (pad dy 2 " ") " "
      (pad h 2 "0") ":" (pad m 2 "0") " "
      name)))

(defun _print-short-list (node memfilter)
  ; (_print-short-list node filter) -> nil
  ; Iterates through short listing of path-node
  ; printing simple content information
  (each (lambda (_el)
    (prtnl _el))
        (. node :all_members nil memfilter)))

(defun _print-long-list (node memfilter)
  ; (_print-long-list node filter) -> nil
  ; Iterates through long listing of path-node
  ; printing simple content information
  (each (lambda (_el)
    (defq fname (. node :fqname _el))
    (_print-long-entry _el (pii-fstat fname)))
        (. node :all_members nil memfilter)))

(defun list-files (ic &optional args)
  ; (list-files internal args) -> nil
  ; Lists files in either cwd or other in argument
  ; emulates Linux/Darwin 'ls' command
  ; Flags include
  ; -l List in long format
  ; -a All, include directory entries whose names begin with a dot (.)
  (bind '(sargs flags paths) (_split-args args))
  (defq
    flgs  (_collapse_flags flags)
    lng?  (gets flgs "l")
    fltr  (if (gets flgs "a") _pn-all-filter _pn_short-filter)
    lstfn (if lng? _print-long-list _print-short-list)
    flist (if (nempty? paths) paths (list ".")))
  (each
    (lambda (_el)
      (defq
        fpath nil
        psplt (_path-tolist _el)
        plen  (dec (length psplt))
        node  (node-for _el _pn_nofind-handler))
      (cond
        ; When an error is found in 'node-for'
        ((list? node)
         (bind '(segname pnode plistndx) node)
         ; If it is the final segment of path
         (if (= plistndx plen)
           (progn
             (defq
              fname (. pnode :fqname segname)
              stat (pii-fstat fname))
             ; And is a file
             (if (and stat (isfile? stat))
               (if lng?
                   (_print-long-entry segname stat)
                   (prtnl segname))
               (prtnl (str "ls: " segname ": No such file or directory"))))
           ; Otherwise it is some segment in path error
           (prtnl (str "ls: " _el ": No such file or directory"))))
        ; When we find a cohesive path-node
        (t
          (lstfn node fltr))))
    flist)
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
