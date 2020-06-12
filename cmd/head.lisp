;imports
(import 'class/lisp.inc)
(import 'cmd/argparse.inc)

(defun get-stream (args alen)
  ; (get-stream args argslen) -> file-stream
  (file-stream (cond
    ((or (= alen 0) (= alen 2)) 'stdin)
    ((= alen 1) (elem 0 args))
    ((= alen 3)
     (if (= (find :count args) 0) (last args) (first args))))))

(defun get-count (args alen)
  ; (get-count args arglen) -> count
  (cond
    ((or (= alen 0) (= alen 1)) 10)
    ((or (= alen 2) (= alen 3))
     (elem (inc (find :count args)) args))))

(defun main-callback (self args)
  (defq
    alen   (length args)
    instrm (get-stream args alen)
    lcnt   (get-count args alen)
    icnt   0)
  (while (and (< icnt lcnt) (defq ln (read-line instrm)))
         (print ln)
         (setq icnt (inc icnt))))

(defun create-parser (argv)
  (defq parser (create-argparse (elem 0 argv) "v0.1" (slice 1 -1 argv)))
  (set-properties parser
                  :help "returns -c{ount} lines from file, defaults to 10 lines"
                  :handler main-callback
                  :validator validate-file-exists
                  :counter 1)

  ; Include line count in result
  (add-argument
    parser
    (set-properties (create-argument
                      '("-c" "--count")
                      "display count of lines")
                    :validator validate-integer
                    :counter 1
                    :dest :count))
  parser)
; Command entry point
(defun main ()
  ;initialize pipe details and command args, abort on error
  (when (defq stdio (create-stdio))
    (defq ap (create-parser (stdio-get-args stdio)))
    (parse ap)))
