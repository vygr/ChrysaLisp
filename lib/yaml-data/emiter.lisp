;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; emmiter - ChrysaLisp Object YAML Emmiter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/nodes.lisp)

; Breaks out properties to explicitly tagged
;   :entries
;     :key
;     :value
; May prommote to 'xtras'
;
; (:clz :me_key :value k)
; (:clz :me_value :value v)
; (:clz :prop-me :entries ((:clz :me_key :value k) (:clz :me_value :value v))
;

(defun me-key? (_)
  (eql (getp _ :clz) :me_key))

(defun me-value? (_)
  (eql (getp _ :clz) :me_value))

(defun prop-me? (_)
  (eql (getp _ :clz) :prop_me))

(defun make-me-key (_)
  (list :clz :me_key :value _))

(defun make-me-value (_)
  (list :clz :me_value :value _))

(defun split-entries (p)
  (reduce
    (lambda (acc el)
      (push acc
        (list :clz :prop_me :entries
              (push
                (push (list) (make-me-key (first el)))
                (make-me-value (second el))))))
    (entries p) (list)))

; Not multi-threaded safe

(defq ywcntrl       nil)

; AST Context Stack

(defun set-context! (n)
  ; (set-context! node) -> node
  ; Makes the current context 'node'
  ; after adding 'node' to current's children
  (defq crn (getp ywcntrl :current))
  ; Stack node in path for un-setting
  (push (getp ywcntrl :path) n)
  ; If first
  (when crn
    (add-child-node! crn n))
  (setp! ywcntrl :current n)
  n)

(defun unset-context! ()
  ; (unset-context!) -> node | nil
  ; Set's context to most recent in path
  (defq
    npath (getp ywcntrl :path)  ; Setup
    lnode (pop npath))          ; Pop path stack
  (when (truthy? npath)
    (setp! ywcntrl :current (last npath)))
  nil)

(defun add-to-context! (n)
  ; (add-to-context! node) -> node | nil
  (when (truthy? (defq crn (getp ywcntrl :current)))
    (add-child-node! crn n)))

; Object walking for building nodes
(defun build-nodes! (odata)
  ; (build-nodes! object-data) -> nil
  ; Generates AST for inbound data
  (cond
    ((lst? odata)
        (cond
          ; Map
          ((props? odata)
           (set-context! (MapNode))
           (each build-nodes! (split-entries odata))
           (unset-context!))
          ; Map Entry
          ((prop-me? odata)
           (set-context! (MapEntryNode))
           (each build-nodes! (getp odata :entries))
           (unset-context!))
          ; Key
          ((me-key? odata)
           (set-context! (KeyNode))
           (build-nodes! (getp odata :value))
           (unset-context!))
          ; Value
          ((me-value? odata)
           (set-context! (ValueNode))
           (build-nodes! (getp odata :value))
           (unset-context!))
          ; Sequence
          (t
            (set-context! (SequenceNode))
            (each build-nodes! odata)
            (unset-context!))))
    ((kw? odata)
     (add-to-context! (ScalarNode :keyword odata)))
    ((sym? odata)
     (add-to-context! (ScalarNode :symbol odata)))
    ((str? odata)
     (add-to-context! (ScalarNode :string odata)))
    ((num? odata)
     (add-to-context! (ScalarNode :number odata)))
    (t
     (throw "Unknown type found in build-nodes!" odata))))


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
  (write (getp ywcntrl :stream) (str (pad-indent) v ": ")))

(defun value-writer (v)
  (write (getp ywcntrl :stream) (str (pad-indent) v (char 0x0a))))

(defun seq-start-writer (ctype)
  (bind '(lnd lindx lcnt) (last-node))
  (bind '(mrc mindx mcnt) (mr-container))
  (defq
    strm (getp ywcntrl :stream)
    ncnt (getp ywcntrl :container-count))
  ; (spit-npath "ssw")
  ; (print "  -> lnd " lnd " mrc " mrc " ctype " ctype)
  (cond
    ; Seq as first child of seq
    ((eql ctype :seq)
     ; (print " ctype= seq puts '-'")
     (default-writer "-" strm))
    ((and (eql lnd :scalar) (eql ctype :scalar))
     ; (print " lnd= scalar ctype= scalar puts '-'")
     (dec-indent)
     (default-writer "-" strm)
     (inc-indent))
    ((and (eql ctype :scalar) (not (eql mrc :seq)))
     ; Next is scalar
     ; (print " ctype= seq not child= seq puts 'cr'")
     (write strm (char 0x0a)))
    ((and (eql lnd :scalar) (eql mrc :seq))
     ; (print " lnd= scalar mrc= seq")
     (default-writer "-" strm))
    ((and (eql lnd :seq) (eql ctype :scalar) (= mcnt ncnt))
     ; (print " lnd= seq ctype= scalar puts ''")
     ; (write strm (char 0x0a))
     )
    ((and (eql lnd :seq) (eql ctype :scalar) (/= mcnt ncnt))
     ; (print " lnd= seq ctype= scalar puts '-' mrc " mcnt " ncnt " ncnt)
     (dec-indent)
     (default-writer "-" strm)
     (inc-indent))
    ; Sequence following scalar
    ; ((and (or
    ;         (eql lnd :scalar)
    ;         (eql lnd :value)
    ;         (eql lnd :seq))
    ;       (or
    ;         (eql ctype :seq)
    ;         (eql mrc :seq)))
    ;  (print " lnd= scalar | value, mrc= seq write")
    ;  (write strm
    ;         (str
    ;           (padp-indent) "-" (char 0x0a)
    ;           ; (pad-indent) "-")
    ;         )))
    ((and (eql ctype :seq) (eql lnd :seq))
     (print " ctype= seq lnd= seq write CRAZY BIRD")
     (write strm
            (str
              (padp-indent) "-" (char 0x0a)
              (pad-indent) "-")))
    ; Sequence following sequence
    ; ((eql lnd :seq)
    ;  (print " lnd= seq write")
    ;  (write strm
    ;         (str
    ;           (padp-indent) "-" (char 0x0a)
    ;           ; (pad-indent) "-")
    ;         )))
     ; (write strm (str (char 0x0a) (pad-indent) "- ")))
    (t
      (throw "Unknown " (list lnd mrc ctype)))))

(defun seq-value-writer (v)
  ; (print "svw " (getp ywcntrl :npath))
  (write (getp ywcntrl :stream) (str (pad-indent) "- " v (char 0x0a))))

(defun node-to-stream (ast &optional pwrt)
  ; (gen-stream stream ast) -> nil
  (setd pwrt default-writer)
  (case (getp ast :type)
    ((:docstart)
     (push-npath :docstart)
     (setp! ywcntrl :indent 0)
     (pwrt (getp ast :value))
     (setp! ywcntrl :indent -1)
     (each node-to-stream (getp ast :children)))
    ((:map)
     (inc-indent)
     (push-npath :map)
     (each node-to-stream (getp ast :children))
     (dec-indent)
     (pop-npath))
    ((:seq)
     (defq fchld
           (getp (first (getp ast :children)) :type))
     (inc-indent)
     (seq-start-writer fchld)
     (push-npath :seq)
     (each (#(node-to-stream %0 seq-value-writer)) (getp ast :children))
     (dec-indent)
     (unwind-to-node :seq)
     (pop-npath))
    ((:map_entry)
     (each node-to-stream (getp ast :children)))
    ((:key)
     (push-npath :key)
     (each (#(node-to-stream %0 key-writer)) (getp ast :children))
     (pop-npath))
    ((:value)
     (push-npath :value)
     (when (find (getp (first (getp ast :children)) :type) (list :seq :map))
         (write (getp ywcntrl :stream) (char 0x0a)))
     (each (#(node-to-stream %0 value-writer)) (getp ast :children))
     (pop-npath))
     ; (each node-to-stream (getp ast :children)))
    ((:scalar)
     (pwrt (getp ast :value))
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
                                  :path  (list)
                                  :current nil
                                  :indent -1)))
  (set-context! (getp ywcntrl :root))
  (build-nodes! data)
  (add-to-context! (DocEndNode))
  (unset-context!)
  ; (print (getp ywcntrl :root))
  (node-to-stream (getp ywcntrl :root))

  ; (walk-tree (getp ywcntrl :root) print)
  ; (pre-walk-recur print (getp ywcntrl :root))
  ; (write stream (str (getp ywcntrl :root)))
  stream)
