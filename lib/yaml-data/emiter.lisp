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
    (prop-entries p) (list)))

; Not multi-threaded safe

(defq ywcntrl       nil)
(defq indent_space  2)
(defq max_level     0)

(defun inc_indent()
  (defq ci (getp ywcntrl :indent))
  (setp! ywcntrl :indent (inc ci)))

(defun dec_indent()
  (defq ci (getp ywcntrl :indent))
  (setp! ywcntrl :indent (dec ci)))

(defun pad_indent()
  (pad "" (* indent_space (getp ywcntrl :indent))))

(defun default-writer (v)
  (write (getp ywcntrl :stream) (str (pad_indent) v (char 0x0a))))

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

; Realize AST to stream

(defun key-writer (v)
  (write (getp ywcntrl :stream)
         (str (pad_indent) v ":")))

(defun value-writer (v)
  (write (getp ywcntrl :stream)
         (str (pad_indent) v (char 0x0a))))

(defun seq-write-start ()
  (write (getp ywcntrl :stream)
         (str (pad_indent) "- " (char 0x0a))))

(defun seq-writer (v)
  (write (getp ywcntrl :stream)
         (str (pad_indent) "- " v (char 0x0a))))

(defun node-to-stream (ast &optional pwrt)
  ; (gen-stream stream ast) -> nil
  (setd pwrt default-writer)
  (case (getp ast :type)
    ((:docstart)
     (setp! ywcntrl :indent 0)
     (pwrt (getp ast :value))
     (setp! ywcntrl :indent -1)
     (each node-to-stream (getp ast :children)))
    ((:map)
     (inc_indent)
     (each node-to-stream (getp ast :children))
     (dec_indent))
    ((:seq)
     (defq
       first_type (getp (first (getp ast :children)) :type)
       indent_lvl (getp ywcntrl :indent))
     (inc_indent)
     (cond
       ((eql first_type :seq)
        (seq-write-start)
        (defq pf 0)
        (each
          (lambda (n)
            (case (getp n :type)
              ((:seq)
               (when (> pf 0)
                 (seq-write-start)
                 (inc_indent))
               (node-to-stream n seq-writer)
               (when (> pf 0)
                 (dec_indent))
               (setq pf (inc pf)))
              (t
                (node-to-stream n seq-writer)))
            ) (getp ast :children)))
       (t
         (each (#(node-to-stream %0 seq-writer)) (getp ast :children))))
     (dec_indent))
    ((:map_entry)
     (each node-to-stream (getp ast :children)))
    ((:key)
     (each (#(node-to-stream %0 key-writer)) (getp ast :children)))
    ((:value)
     (when (find (getp (first (getp ast :children)) :type) (list :seq :map))
         (write (getp ywcntrl :stream) (char 0x0a)))
     (each (#(node-to-stream %0 value-writer)) (getp ast :children)))
     ; (each node-to-stream (getp ast :children)))
    ((:scalar)
     (pwrt (getp ast :value)))
    ((:docend)
     (setp! ywcntrl :indent 0)
     (pwrt (getp ast :value)))
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
