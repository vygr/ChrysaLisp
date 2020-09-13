;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constructor - ChrysaLisp YAML Object Constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)

(defun verify-ctype (node ctype)
  (if (eql (first node) ctype)
      t
      (throw (str "Expected '" ctype "' found " (first node)))))

(defun construct-scalar (acc node)
  (push acc node))

(defun construct-key (acc node)
  (cdisp acc (first node)))

(defun construct-value (acc node)
  (cdisp acc (first node)))

(defun construct-entry (acc node)
  ; (print "    entry node start " (first (first node)))
  (defq
    fnode (first node)
    ftype (first fnode))

  (cond
    ((eql ftype :scalar) (push acc (second fnode)))
    ((eql ftype :list) (cdisp acc fnode))
    (t  (defq l (list))
        (each (lambda (n) (cdisp l n)) node)
        (push acc l))))

(defun construct-list (acc node)
  (defq l (list))
  (each (lambda (n) (cdisp l n)) node)
  (push acc l))

(defun construct-properties (acc node)
  (defq
    p (properties)
    l (list))
  (each (lambda (n) (cdisp l n)) node)
  (each (lambda (e) (setp! p (first e) (second e) t)) l)
  (push acc p))

(defq cjmp
  (properties
    :scalar     construct-scalar
    :key        construct-key
    :value      construct-value
    :entry      construct-entry
    :list       construct-list
    :properties construct-properties))

(defun cdisp (acc el)
  (defq fn (getp cjmp (first el)))
  (fn acc (second el)))

(defun construct-document (node)
  (verify-ctype node :document)
  (defq container (list))
  (each (lambda (_) (cdisp container _)) (second node))
  container)

(defun-bind construct (ast options)
  ; (print "Construct")
  ; (print ast)
  (each construct-document ast))

