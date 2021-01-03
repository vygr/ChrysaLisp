;imports
(import "class/lisp.inc")
; (import "lib/argparse/argparse.inc")
(import "lib/argparse/argpclz.lisp")

(defun get-stream (arg)
	; (get-stream arg) -> stream
	(if (str? arg)
		(file-stream arg)
		(io-stream arg)))

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


(defun create-parser ()
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
               :dest      'file)))
  (. parser :add_action
     (bool-switch "-l" (emap-kv :dest 'lines)))
  (. parser :add_action
     (bool-switch "-w" (emap-kv :dest 'words)))
  (. parser :add_action
     (bool-switch "-c" (emap-kv :dest 'characters)))
  parser)

(defun perform (_cmd)
  (print (entries _cmd))
  (print "File is of type " (type-of (gets _cmd 'file)))
  )

; Command entry point
(defun main ()
  ;initialize pipe details and command args, abort on error
  (when (defq stdio (create-stdio))
    (defq
      args  (stdio-get-args stdio)
      ap    (create-parser))
    ; (parse ap)
    (when (defq res (. ap :parse args))
      (perform res))))
