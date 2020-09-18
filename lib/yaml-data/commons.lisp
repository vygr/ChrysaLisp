;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; commons - ChrysaLisp YAML and xchange common functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; AST Node Context Stack

(defun Context ()
  ; (Context) -> properties
  ; Constructs a common node stack context
  (properties
    :current nil
    :path    (list)))

(defun set-context! (cntxt n)
  ; (set-context! node) -> node
  ; Makes the current context 'node'
  ; after adding 'node' to current's children
  (defq crn (getp cntxt :current))
  ; Stack node in path for un-setting
  (push (getp cntxt :path) n)
  ; If first
  (when crn
    (add-child-node! crn n))
  (setp! cntxt :current n)
  n)

(defun unset-context! (cntxt)
  ; (unset-context!) -> node | nil
  ; Set's context to most recent in path
  (defq
    npath (getp cntxt :path)  ; Setup
    lnode (pop npath))          ; Pop path stack
  (when (truthy? npath)
    (setp! cntxt :current (last npath)))
  nil)

(defun add-to-context! (cntxt n)
  ; (add-to-context! node) -> node | nil
  (when (truthy? (defq crn (getp cntxt :current)))
    (add-child-node! crn n)))

(defun obj-to-node (cntxt odata)
  ; (obj-to-node Context object)
  ; Builds an common Node AST from object
  (cond
    ((lst? odata)
        (cond
          ; Map
          ((props? odata)
           (set-context! cntxt (MapNode))
           (each (#(obj-to-node cntxt %0)) (split-entries odata))
           (unset-context! cntxt))
          ; Map Entry
          ((prop-me? odata)
           (set-context! cntxt (MapEntryNode))
           (each (#(obj-to-node cntxt %0)) (getp odata :entries))
           (unset-context! cntxt))
          ; Key
          ((me-key? odata)
           (set-context! cntxt (KeyNode))
           (obj-to-node cntxt (getp odata :value))
           (unset-context! cntxt))
          ; Value
          ((me-value? odata)
           (set-context! cntxt (ValueNode))
           (obj-to-node cntxt (getp odata :value))
           (unset-context! cntxt))
          ; Sequence
          (t
            (set-context! cntxt (SequenceNode))
            (each (#(obj-to-node cntxt %0)) odata)
            (unset-context! cntxt))))
    ((kw? odata)
     (add-to-context! cntxt (ScalarNode :keyword odata)))
    ((sym? odata)
     (add-to-context! cntxt (ScalarNode :symbol odata)))
    ((str? odata)
     (add-to-context! cntxt (ScalarNode :string odata)))
    ((num? odata)
     (add-to-context! cntxt (ScalarNode :number odata)))
    (t
     (throw "Unknown type found in obj-to-node" odata))))

