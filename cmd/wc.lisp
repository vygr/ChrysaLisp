(import "class/lisp.inc")
(import "lib/argparse/argpclz.lisp")

(defun arg-to-stream (arg)
	; (get-stream arg) -> stream
 (cond
   ((sym? arg)
    (io-stream arg))
   (t
     (file-stream arg))))

(defun line-count (res ln)
  ; (line-count res ln) -> ln
  (sets! res :lines (inc (gets res :lines)))
  ln)

(defun word-count (res ln)
  ; (word-count res ln) -> ln
  (sets! res :words (+ (gets res :words) (length (split ln " "))))
  ln)

(defun char-count (res ln)
  ; (char-count res ln) -> ln
  (sets! res :characters (+ (gets res :characters) (length ln)))
  ln)

(defun perform (_cmd)
  ; (perform parsed_argument_map)
  (defq
    results (emap-kv :lines 0 :words 0 :characters 0)
    instrm  (arg-to-stream (gets _cmd 'file))
    rnlist  (reduce
              (lambda (acc (_k _v))
                (cond
                  ((and (eql _k 'lines) _v)
                   (push acc (curry line-count results)))
                  ((and (eql _k 'words) _v)
                   (push acc (curry word-count results)))
                  ((and (eql _k 'characters) _v)
                   (push acc (curry char-count results)))
                  (t acc)))
              (entries _cmd) (list)))

  (when (/= (length rnlist) 0)
    (while (defq ln (read-line instrm))
            ((apply compose rnlist) ln)))
  (print (entries results)))

(defun create-parser ()
  ; (create-parser)
  ; Setup the command line parser
  (defq
    parser (argparse "wc"
                     (emap-kv
                       :version "v0.1"
                       :help "word counter")))
  (. parser :add_action
     (switch "-f"
             (emap-kv
               :type      :file
               :default   'stdin
               :validate  :file_exist
               :help      "file to summarize"
               :dest      'file)))
  (. parser :add_action
     (bool-switch "-l"
                  (emap-kv
                    :dest 'lines
                    :help "include line count in summary")))
  (. parser :add_action
     (bool-switch "-w"
                  (emap-kv
                    :dest 'words
                    :help "include word count in summary")))
  (. parser :add_action
     (bool-switch "-c"
                  (emap-kv
                    :dest 'characters
                    :help "include character count in summary")))
  parser)

; Command entry point
(defun main ()
  ;initialize pipe details and command args, abort on error
  (when (defq stdio (create-stdio))
    (defq
      args  (stdio-get-args stdio)
      ; stdin (io-stream 'stdin)
      ap    (create-parser))
    ; (parse ap)
    (when (defq res (. ap :parse args))
      (perform res)))
  0)
