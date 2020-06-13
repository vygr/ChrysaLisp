;imports
(import 'class/lisp.inc)
(import 'cmd/argparse.inc)

(defun get-stream (args alen)
  ; (get-stream args argslen) -> file-stream
  (file-stream
    (cond
      ((= alen 1) (first args))
      ((<= 0 alen 2) 'stdin)
      (t (if (= (find :count args) 0)
             (last args)
             (first args))))))

(defun get-count (args alen)
  ; (get-count args arglen) -> count
  (if (<= alen 1) 10 (elem (inc (find :count args)) args)))

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
  (defq parser (create-argparse (first argv) "v0.1" (rest argv)))
  (set-properties parser
                  :help "returns lines from beginning of file, defaults to first 10 lines"
                  :handler main-callback
                  :validator validate-file-exists
                  :counter 1)

  ; Include line count in result
  (add-argument
    parser
    (set-properties (create-argument
                      '("-c" "--count")
                      "count of lines from top of file to display")
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
