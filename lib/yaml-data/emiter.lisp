;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; emmiter - ChrysaLisp Object YAML Emmiter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/nodes.lisp)

; Not multi-threaded safe

(defq ywcntrl nil)

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

(defun build-nodes! (idata)
  ; (build-nodes! idata) -> nil
  ; Generates AST for inbound data
  (cond
    ((lst? idata)
        (cond
          ; Map
          ((props? idata)
           (set-context! (MapNode))
           (each build-nodes! (split-entries idata))
           (unset-context!))
          ; Map Entry
          ((prop-me? idata)
           (set-context! (MapEntryNode))
           (each build-nodes! (getp idata :entries))
           (unset-context!))
          ; Key
          ((me-key? idata)
           (set-context! (KeyNode))
           (build-nodes! (getp idata :value))
           (unset-context!))
          ; Value
          ((me-value? idata)
           (set-context! (ValueNode))
           (build-nodes! (getp idata :value))
           (unset-context!))
          ; Sequence
          (t
            (set-context! (SequenceNode))
            (each build-nodes! idata)
            (unset-context!))))
    ((kw? idata)
     (add-to-context! (ScalarNode :keyword idata)))
    ((sym? idata)
     (add-to-context! (ScalarNode :symbol idata)))
    ((str? idata)
     (add-to-context! (ScalarNode :string idata)))
    ((num? idata)
     (add-to-context! (ScalarNode :number idata)))
    (t
     (throw "Unknown type found in build-nodes!" idata))))

(defun-bind emit (stream data in-args)
  ; (emit stream data options) -> stream
  ; Converts data to strings and writes to streams
  (setq ywcntrl (pmerge in-args (properties
                                  :level  -1
                                  :indent -1
                                  :stream stream
                                  :root  (DocStartNode)
                                  :path  (list)
                                  :current nil)))
  (set-context! (getp ywcntrl :root))
  (build-nodes! data)
  (add-to-context! (DocEndNode))
  (unset-context!)
  ; (walk-tree (getp ywcntrl :root) print)
  ; (pre-walk-recur print (getp ywcntrl :root))
  ; (write stream (str (getp ywcntrl :root)))
  stream)
