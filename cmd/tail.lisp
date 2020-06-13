;imports
(import 'class/lisp.inc)
(import 'cmd/argparse.inc)

(defun get-stream (args alen)
  ; (get-stream args argslen) -> file-stream
  (file-stream
    (cond
      ((or (= alen 0) (= alen 2)) 'stdin)
      ((= alen 1) (elem 0 args))
      (t (if (= (find :count args) 0)
             (last args)
             (first args))))))

(defun get-count (args alen)
  ; (get-count args arglen) -> count
  (cond
    ((or (= alen 0) (= alen 1)) 10)
    (t (elem (inc (find :count args)) args))))

(defun main-callback (self args)
  (defq
    alen   (length args)
    instrm (get-stream args alen)
    lcnt   (get-count args alen)
    fllist (list))
    (while (defq ln (read-line instrm))
           (push fllist ln))
    (let ((fcnt (length fllist))
          (scnt 0) (ecnt 0))
        (if (< fcnt lcnt)
            (setq ecnt fcnt)
            (setq scnt (- fcnt lcnt)
                  ecnt fcnt))
        (each print (slice scnt ecnt fllist))))


(defun create-parser (argv)
  (defq parser (create-argparse (elem 0 argv) "v0.1" (slice 1 -1 argv)))
  (set-properties parser
                  :help "returns lines from end of file, defaults to last 10 lines"
                  :handler main-callback
                  :validator validate-file-exists
                  :counter 1)

  ; Include line count in result
  (add-argument
    parser
    (set-properties (create-argument
                      '("-c" "--count")
                      "count of lines from bottom of file to display")
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
