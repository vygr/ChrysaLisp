;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reflect - Reflection Library
; Warning - Subject to the nuanced changes to
; the `(defclass ...) macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A class declaration is a lambda so the meta info
; deconstructs the lambda to get at the interesting bits

(defmacro _mclass-args  (_) `(elem-get 1 ,_))  ; Position of class constructor args
(defmacro _mclass-decl  (_) `(elem-get 2 ,_))  ; Position of declaration info
(defmacro _mclass-body  (_) `(elem-get 3 ,_))  ; Position of class body
(defmacro _mclass-vars  (_) `(elem-get 4 ,_))  ; Position of class initializers
(defmacro _mclass-initblk (_) `(second (_mclass-decl ,_)))
(defmacro _mclass-init (_)    `(last (_mclass-initblk ,_)))
(defmacro _mclass-id (_)      `(last (_mclass-decl ,_)))
(defmacro _mclass-meths (_)   `(rest (last (_mclass-body ,_))))
(defmacro _mclass-super (_)   `(first (_mclass-init ,_)))

; An instance meta data is a data block containing
; the super vtable, module information and methods
(defmacro _minst-vtable (_) `(elem-get 0 ,_))
(defmacro _minst-module (_) `(second (elem-get 1 ,_)))

; Functions on class declarations

(defmacro class-inherits? (_)
  ; (class-inherits symbol) -> t | nil
  ; Predicate for answering if class inherits
  ; from another class
  `(list? (_mclass-super ,_)))

(defmacro _mclass-module (_)
  ; (_mclass-module classname) -> str
  ; Pulls the module name from class
  `(last
    (elem
      (if (class-inherits? ,_) 2 1)
      (elem-get 2 (_mclass-body ,_)))))

(defmacro class-hasvars? (_)
  ; (class-hasvars? symbol) -> t | nil
  ; Predicate for answering if a class declares
  ; variables
  `(list? (_mclass-vars ,_)))

(defmacro class? (_)
  ; (class? symbol) -> t | nil
  ; Predicate for answering if a symbol is of type 'class'
  `(if (and
    (seq? (defq ,(defq __ (gensym)) ,_))
    (> (length ,__) 2)
    (list? (_mclass-decl ,__)))
    (starts-with "*class" (_mclass-id ,__))))

(defun classkw (_)
  ; (classkw sym) -> kw
  ; Returns the class keyword from the class declaration
  (cond
    ((class? _)
      (sym (str ":" (slice 0 -2 (last (split (_mclass-id _) "_"))))))
    (t
      :NOT_A_CLASS)))

; Profile

(defun assignment (_)
  (cond
    ((and (seq? _) (class? (first _)))
      (classkw (first _)))
    ((find (last (type-of _)) (list :str :num :sym))
      _)
    ((or (func? _) (lambda? _) (macro? _))
      :form)
    ((seq? _)
      :expression)
    (t
      :unknown)))

(defun inspect-class (_ &optional sindent)
  ; (inspect-class symbol [indent width]) -> nil
  ; Recursive class walker
  (when (class? _)
    (setd sindent 0)
    (defq ind (pad "" sindent))
    (print ind (_mclass-id _) " " (_mclass-module _))
    (when (class-inherits? _)
      (inspect-class (_mclass-super _) (+ sindent 2)))
    (print ind "  constructor: ")
    (print ind "    -> " (_mclass-args _))
    (print ind "  variables: ")
    (when (class-hasvars? _)
      (each (lambda ((_f _v))
        (print
          ind "     "
          _f " <- " (assignment _v)))
      (rest (partition 2 (_mclass-vars _)))))
    (print ind "  methods:")
    ; There is at least a :type-of method
    (each (lambda (vf)
      (when (list? (second vf))
        (print ind "     " (elem-get 2 vf)))) (_mclass-meths _)))
  nil)

; Functions on class instances
(defmacro class-inst? (_)
  ; (class-inst? object) -> t | nil
  ; Predicate for answering if the object is of type class
  (if (sym? _)
  `(and (env? ,_) (def? :vtable ,_))
  `(and (env? (defq ,(defq __ (gensym)) ,_)) (def? :vtable ,__))))

(defmacro class-type (_)
  ; (class-type object) -> keyword | nil
  ; Returns the class of an object or
  ; nil if not class
  `(if (class-inst? (defq ,(defq __ (gensym)) ,_))
    (last (. ,__ :type_of))))

(defmacro class-type-of? (obj ttype)
  ; (type-of? object ctype) -> t | nil
  ; Predicate for answering if object is of a certain class type
  `(progn
    (defq
      ,(defq _ttype (gensym)) ,ttype
      ,(defq _obj (gensym)) ,obj)
    (cond
    ((find ,_ttype (type-of ,_obj)) t)
    ((class-inst? ,_obj) (find ,_ttype (. ,_obj :type_of)))
    (t nil))))

(defun inst-data (_)
  ; (inst-data object) -> list
  ; Returns a list of data/value pairs (not including :vtable)
  ; if the object is a class instance
  (when (class-inst? _)
    (filter
      (lambda ((_k _v))
        (not (eql _k :vtable))) (tolist _))))
