;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tuiutils - Terminal utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +tenv-file+ "apps/terminal/.hostenv" )

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

(defun split-args (args)
  ; (split-args string) -> list
  ; Splits flags from arguments
  (defq sargs (split args " "))
  (reduce
    (lambda (acc el)
      (if (eql (first el) "-")
          (push (second acc) el)
          (push (last acc) el)) acc) sargs (list sargs (list) (list))))

(defun not-impl (ic &optional args)
  (prtnl (str ic " -> not implemented")))

(defun copy-file (ic &optional args)
  (bind '(sargs flags paths) (split-args args))
  (not-impl ic))

(defun move-file (ic &optional args)
  (bind '(sargs flags paths) (split-args args))
  (not-impl ic))

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
  (defq +source-host-env+ ".hostenv")
  (when (= (age +source-host-env+) 0)
    (throw "Host environment file not found " +source-host-env+))
  (cond
    ((= (age +tenv-file+) 0)
     (save-envmap
       (sets-pairs!
         (load-envmap +source-host-env+)
         "PROMPT" ">"
         "PATH" "apps:cmd")
       +tenv-file+))
    (t
      (load-envmap +tenv-file+))))