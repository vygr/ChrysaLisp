;imports
(import "class/lisp.inc")
(import "lib/argparse/argparse.inc")

(defun get-stream (args)
	;(get-stream args) -> stream
	(if (find :file_name args)
		(file-stream (elem (inc (find :file_name args)) args))
		(io-stream 'stdin)))

(defun line-count (res ln)
  ; (line-count res ln) -> ln
  (defq
    lndx (inc (find :lines res)))
  (elem-set lndx res (inc (elem lndx res)))
  ln)

(defun word-count (res ln)
  ; (word-count res ln) -> ln
  (defq
    lndx (inc (find :words res)))
  (elem-set lndx res (+ (elem lndx res) (length (split ln " "))))
  ln)

(defun char-count (res ln)
  ; (char-count res ln) -> ln
  (defq
    lndx (inc (find :characters res)))
  (elem-set lndx res (+ (elem lndx res) (length ln)))
  ln)

; Designated handler for main processor
(defun main-callback (self args)
  ; (main-callback self args) -> results
  (defq
    results (list :lines 0 :words 0 :characters 0)
    instrm (get-stream args)
    rnlist (reduce (lambda (acc el)
                     (cond
                       ((eql el :lines)
                         (push acc (curry line-count results)))
                       ((eql el :words)
                         (push acc (curry word-count results)))
                       ((eql el :chars)
                         (push acc (curry char-count results)))
                       (t
                         acc))) args (list)))
  (when (/= (length rnlist) 0)
    (while (defq ln (read-line instrm))
            ((apply compose rnlist) ln)))
  (print results))

(defun create-parser (argv)
  (defq parser (create-argparse (elem 0 argv) "v0.1" (slice 1 -1 argv)))
  (set-properties parser
                  :help "word counter"
                  :handler main-callback
                  :validator validate-none
                  :counter +no_count+)

  ; File name argument
  (add-argument
    parser
    (set-properties (create-argument
                      '("-f" "--file")
                      "file input to wc")
                    :type :type_file
                    :validator validate-file-exists
                    :counter 1
                    :dest :file_name))
  ; Include line count in result
  (add-argument
    parser
    (set-properties (create-argument
                      '("-l" "--lines")
                      "display count of lines")
                    :validator validate-none
                    :dest :lines))
  ; Include word count in result
  (add-argument
    parser
    (set-properties (create-argument
                      '("-w" "--words")
                      "display count of words")
                    :validator validate-none
                    :dest :words))
  ; Include char count in result
  (add-argument
    parser
    (set-properties (create-argument
                      '("-c" "--chars")
                      "display count of characters")
                    :validator validate-none
                    :dest :chars))
  parser)

; Command entry point
(defun main ()
  ;initialize pipe details and command args, abort on error
  (when (defq stdio (create-stdio))
    (defq ap (create-parser (stdio-get-args stdio)))
    (parse ap)))
