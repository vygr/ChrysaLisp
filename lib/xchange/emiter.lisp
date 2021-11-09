;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; emmiter - ChrysaLisp Object YAML Emmiter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")

; Not multi-threaded safe

(defq ywcntrl nil)

; Node path control

(defq container-nodes (list :docstart :seq :map :key :value))

(defun push-npath (n)
  (defq npth (gets ywcntrl :npath))
  (defq
    ccount (inc (gets ywcntrl :container-count))
    cindex (length npth))
  (push npth (list n cindex ccount))
  (sets-pairs! ywcntrl
          :container-count ccount
          :container-index cindex))

(defun pop-npath ()
  (sets! ywcntrl
         :container-index (dec (gets ywcntrl :container-index)))
  (pop (gets ywcntrl :npath)))

(defun last-node ()
  (last (gets ywcntrl :npath)))

(defun unwind-to-node (ntype)
  (defq keep-going t)
  (while keep-going
    (bind '(nt _ _) (last-node))
    (if (eql nt ntype)
      (setq keep-going nil)
      (pop-npath))))

(defun mr-container ()
  (defq
    res  nil
    np   (gets ywcntrl :npath)
    rpos (dec (length np)))
  (while (>= rpos 0)
    (defq n (elem-get rpos np))
    (if (find (first n) container-nodes)
      (setq res n rpos -1)
      (setq rpos (dec rpos))))
  res)

(defun spit-npath (header)
  (prin header " ")
  (each (#(prin (first %0) ", ")) (gets ywcntrl :npath))
  (print))

; Writer utilities

(defq indent_space  2)

(defun inc-indent()
  (defq ci (gets ywcntrl :indent))
  (sets! ywcntrl :indent (inc ci)))

(defun dec-indent()
  (defq ci (gets ywcntrl :indent))
  (sets! ywcntrl :indent (dec ci)))

(defun pad-indent()
  (pad "" (* indent_space (gets ywcntrl :indent))))

(defun padp-indent()
  (pad "" (* indent_space (dec (gets ywcntrl :indent)))))

; Writers

(defun default-writer (v &optional strm)
  (setd strm (gets ywcntrl :stream))
  (write strm (str (pad-indent) v (char 0x0a))))

(defun key-writer (v)
  (write (gets ywcntrl :stream) (str (pad-indent) (rest v) ": ")))

(defun value-writer (v)
  (write (gets ywcntrl :stream) (str (pad-indent) v (char 0x0a))))

(defun seq-start-writer (ctype)
  (bind '(lnd lindx lcnt) (last-node))
  (bind '(mrc mindx mcnt) (mr-container))
  (defq
    strm (gets ywcntrl :stream)
    ncnt (gets ywcntrl :container-count))
  (cond
    ; Seq as first child of seq
    ((eql ctype :seq)
     (default-writer "-" strm))
    ((and (eql lnd :scalar) (eql ctype :scalar))
     (dec-indent)
     (default-writer "-" strm)
     (inc-indent))
    ((and (eql ctype :scalar) (not (eql mrc :seq)))
     (write strm (char 0x0a)))
    ((and (eql lnd :scalar) (eql mrc :seq))
     (default-writer "-" strm))
    ((and (eql lnd :seq) (eql ctype :scalar) (= mcnt ncnt)))
    ((and (eql lnd :seq) (eql ctype :scalar) (/= mcnt ncnt))
     (dec-indent)
     (default-writer "-" strm)
     (inc-indent))
    (t
      (throw "Unknown " (list lnd mrc ctype)))))

(defun seq-value-writer (v)
  ; (print "svw " (gets ywcntrl :npath))
  (write (gets ywcntrl :stream) (str (pad-indent) "- " v (char 0x0a))))

(defun node-to-yaml-stream (ast &optional pwrt)
  ; (gen-stream stream ast) -> nil
  (setd pwrt default-writer)
  (case (gets ast :type)
    ((:docstart)
     (push-npath :docstart)
     (sets! ywcntrl :indent 0)
     (pwrt (gets ast :value))
     (sets! ywcntrl :indent -1)
     (each node-to-yaml-stream (gets ast :children)))
    ((:map)
     (inc-indent)
     (push-npath :map)
     (each node-to-yaml-stream (gets ast :children))
     (dec-indent)
     (pop-npath))
    ((:seq)
     (defq fchld
           (gets (first (gets ast :children)) :type))
     (inc-indent)
     (seq-start-writer fchld)
     (push-npath :seq)
     (each (#(node-to-yaml-stream %0 seq-value-writer)) (gets ast :children))
     (dec-indent)
     (unwind-to-node :seq)
     (pop-npath))
    ((:map_entry)
     (each node-to-yaml-stream (gets ast :children)))
    ((:me_key)
     (push-npath :key)
     (each (#(node-to-yaml-stream %0 key-writer)) (gets ast :children))
     (pop-npath))
    ((:me_value)
     (push-npath :value)
     (when (find (gets (first (gets ast :children)) :type) (list :seq :map))
         (write (gets ywcntrl :stream) (char 0x0a)))
     (each (#(node-to-yaml-stream %0 value-writer)) (gets ast :children))
     (pop-npath))
    ((:scalar)
     (if (eql (gets ast :stype) :boolean)
      (pwrt (if (gets ast :value) "true" "false"))
      (pwrt (gets ast :value)))
     (push-npath :scalar))
    ((:docend)
     (sets! ywcntrl :indent 0)
     (pwrt (gets ast :value))
     (pop-npath))
    (t
      (throw "yaml-emit: Unknown Node Type" (entries ast))))
  nil)

(defun emit (stream data in-args)
  ; (emit stream data options) -> stream
  ; Converts data to strings and writes to streams
  (setq ywcntrl (merges in-args (properties
                                  :stream stream
                                  :root  (DocStartNode)
                                  :npath (list)
                                  :container-index 0
                                  :container-count 0
                                  :context (Context)
                                  :indent -1)))
  ; Setup the context stack with root
  (set-context! (gets ywcntrl :context) (gets ywcntrl :root))
  ; Convert object to node tree
  (obj-to-node (gets ywcntrl :context) data)
  ; Add in the Document end
  (add-to-context! (gets ywcntrl :context) (DocEndNode))
  ; Pop the final stack entry
  (unset-context! (gets ywcntrl :context))
  ; Write to the stream
  (node-to-yaml-stream (gets ywcntrl :root))
  stream)
