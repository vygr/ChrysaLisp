;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; emmiter - ChrysaLisp Object YAML Emmiter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")

; Not multi-threaded safe

(defq ywcntrl nil)

; Node path control

(defq container-nodes (list :docstart :seq :map :key :value))

(defun push-npath (n)
  (defq npth (getp ywcntrl :npath))
  (defq
    ccount (inc (getp ywcntrl :container-count))
    cindex (length npth))
  (push npth (list n cindex ccount))
  (setsp! ywcntrl
          :container-count ccount
          :container-index cindex))

(defun pop-npath ()
  (setp! ywcntrl
         :container-index (dec (getp ywcntrl :container-index)))
  (pop (getp ywcntrl :npath)))

(defun last-node ()
  (last (getp ywcntrl :npath)))

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
    np   (getp ywcntrl :npath)
    rpos (dec (length np)))
  (while (>= rpos 0)
    (defq n (elem rpos np))
    (if (find (first n) container-nodes)
      (setq res n rpos -1)
      (setq rpos (dec rpos))))
  res)

(defun spit-npath (header)
  (prin header " ")
  (each (#(prin (first %0) ", ")) (getp ywcntrl :npath))
  (print))

; Writer utilities

(defq indent_space  2)

(defun inc-indent()
  (defq ci (getp ywcntrl :indent))
  (setp! ywcntrl :indent (inc ci)))

(defun dec-indent()
  (defq ci (getp ywcntrl :indent))
  (setp! ywcntrl :indent (dec ci)))

(defun pad-indent()
  (pad "" (* indent_space (getp ywcntrl :indent))))

(defun padp-indent()
  (pad "" (* indent_space (dec (getp ywcntrl :indent)))))

; Writers

(defun default-writer (v &optional strm)
  (setd strm (getp ywcntrl :stream))
  (write strm (str (pad-indent) v (char 0x0a))))

(defun key-writer (v)
  (write (getp ywcntrl :stream) (str (pad-indent) (rest v) ": ")))

(defun value-writer (v)
  (write (getp ywcntrl :stream) (str (pad-indent) v (char 0x0a))))

(defun seq-start-writer (ctype)
  (bind '(lnd lindx lcnt) (last-node))
  (bind '(mrc mindx mcnt) (mr-container))
  (defq
    strm (getp ywcntrl :stream)
    ncnt (getp ywcntrl :container-count))
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
    ; ((and (eql ctype :seq) (eql lnd :seq))
    ;  (print " ctype= seq lnd= seq write CRAZY BIRD")
    ;  (write strm
    ;         (str
    ;           (padp-indent) "-" (char 0x0a)
    ;           (pad-indent) "-")))
    (t
      (throw "Unknown " (list lnd mrc ctype)))))

(defun seq-value-writer (v)
  ; (print "svw " (getp ywcntrl :npath))
  (write (getp ywcntrl :stream) (str (pad-indent) "- " v (char 0x0a))))

(defun node-to-yaml-stream (ast &optional pwrt)
  ; (gen-stream stream ast) -> nil
  (setd pwrt default-writer)
  (case (getp ast :type)
    ((:docstart)
     (push-npath :docstart)
     (setp! ywcntrl :indent 0)
     (pwrt (getp ast :value))
     (setp! ywcntrl :indent -1)
     (each node-to-yaml-stream (getp ast :children)))
    ((:map)
     (inc-indent)
     (push-npath :map)
     (each node-to-yaml-stream (getp ast :children))
     (dec-indent)
     (pop-npath))
    ((:seq)
     (defq fchld
           (getp (first (getp ast :children)) :type))
     (inc-indent)
     (seq-start-writer fchld)
     (push-npath :seq)
     (each (#(node-to-yaml-stream %0 seq-value-writer)) (getp ast :children))
     (dec-indent)
     (unwind-to-node :seq)
     (pop-npath))
    ((:map_entry)
     (each node-to-yaml-stream (getp ast :children)))
    ((:key)
     (push-npath :key)
     (each (#(node-to-yaml-stream %0 key-writer)) (getp ast :children))
     (pop-npath))
    ((:value)
     (push-npath :value)
     (when (find (getp (first (getp ast :children)) :type) (list :seq :map))
         (write (getp ywcntrl :stream) (char 0x0a)))
     (each (#(node-to-yaml-stream %0 value-writer)) (getp ast :children))
     (pop-npath))
    ((:scalar)
     (if (eql (getp ast :stype) :boolean)
      (pwrt (if (getp ast :value) "true" "false"))
      (pwrt (getp ast :value)))
     (push-npath :scalar))
    ((:docend)
     (setp! ywcntrl :indent 0)
     (pwrt (getp ast :value))
     (pop-npath))
    (t
      ; (throw "yaml-emit: Unknown Node Type" (getp ast :type))
      )
    )
  nil)

(defun-bind emit (stream data in-args)
  ; (emit stream data options) -> stream
  ; Converts data to strings and writes to streams
  (setq ywcntrl (pmerge in-args (properties
                                  :stream stream
                                  :root  (DocStartNode)
                                  :npath (list)
                                  :container-index 0
                                  :container-count 0
                                  :context (Context)
                                  :indent -1)))
  ; Setup the context stack with root
  (set-context! (getp ywcntrl :context) (getp ywcntrl :root))
  ; Convert object to node tree
  (obj-to-node (getp ywcntrl :context) data)
  ; Add in the Document end
  (add-to-context! (getp ywcntrl :context) (DocEndNode))
  ; Pop the final stack entry
  (unset-context! (getp ywcntrl :context))
  ; Write to the stream
  (node-to-yaml-stream (getp ywcntrl :root))
  stream)
