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
  (defq
    l (list))
  (each
    (lambda (n)
      (cdisp l n)) node)
  (push acc l))

(defun construct-list (acc node)
  (defq
    l (list))
  (each
    (lambda (n)
      (cdisp l n)) node)
  (push acc (map (lambda (e) (first e)) l)))

(defun construct-properties (acc node)
  (defq
    p (properties)
    l (list))
  (each
    (lambda (n)
      (cdisp l n)) node)
  (each
    (lambda (e)
      (setp! p (first e) (second e) t)) l)
  (push acc p))

(defq cjmp
  (properties
    :scalar construct-scalar
    :key    construct-key
    :value  construct-value
    :entry  construct-entry
    :list   construct-list
    :properties construct-properties))

(defun cdisp (acc el)
  (defq fn (getp cjmp (first el)))
  (fn acc (second el)))

(defun construct-document (node)
  (verify-ctype node :document)
  (each (curry cdisp (list)) (second node)))

(defun-bind construct (ast options)
  ; (print ast)
  (each construct-document ast))
