;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reflect - Reflection Library
; Warning - Subject to the nuanced changes to
; the `(defclass ...) macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
  +class_prefix+ "*class"
  +scalar_types+ (xset-k :str :num :sym))

; A class declaration is a lambda so the meta info
; deconstructs the lambda to get at the interesting bits

(defmacro _mclass-args (_) `(elem 1 ,_))  ; Position of class constructor args
(defmacro _mclass-decl (_) `(elem 2 ,_))  ; Position of declaration info
(defmacro _mclass-body (_) `(elem 3 ,_))  ; Position of class body
(defmacro _mclass-vars (_) `(elem 4 ,_))  ; Position of class initializers

(defmacro _mclass-initblk (_) `(second (_mclass-decl ,_)))
(defmacro _mclass-init (_)    `(last (_mclass-initblk ,_)))
(defmacro _mclass-id (_)      `(last (_mclass-decl ,_)))
(defmacro _mclass-meths (_)   `(rest (last (_mclass-body ,_))))
(defmacro _mclass-super (_)   `(first (_mclass-init ,_)))

; Functions on class declarations

(defmacro class-inherits? (_)
  ; (class-inherits symbol) -> t | nil
  ; Predicate for answering if class inherits
  ; from another class
  `(list? (_mclass-super ,_)))

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
      (starts-with +class_prefix+ (_mclass-id ,__))))

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
    ((gets +scalar_types+ (last (type-of _)))
      _)
    ((or (func? _) (lambda? _) (macro? _))
      :form)
    ((seq? _)
      :sequence)
    (t
      :unknown)))

(defun inspect-class (_ &optional sindent)
  ; (inspect-class symbol [indent width]) -> nil
  ; Recursive class walker
  (when (class? _)
    (setd sindent 0)
    (defq ind (pad "" sindent))
    (print ind (_mclass-id _))
    (when (class-inherits? _)
      (inspect-class (_mclass-super _) (+ sindent 2)))
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
        (print ind "     " (elem 2 vf)))) (_mclass-meths _)))
  nil)

; Functions on class instances
(defmacro class-inst? (_)
  ; (class-inst? object) -> t | nil
  ; Predicate for answering if the object is of type class
  `(and (env? (defq ,(defq __ (gensym)) ,_)) (def? :vtable ,__)))
  ; (cond
  ;   ((and (env? (defq ,(defq __ (gensym)) ,_)) (def? :vtable ,__))
  ;     (get :type_of (get :vtable ,__)))
  ;   (t nil)))

(defmacro class-type (_)
  ; (class-type object) -> keyword | nil
  ; Returns the class of an object or
  ; nil if not class
  `(if (class-inst? (defq ,(defq __ (gensym)) ,_))
    (last (. ,__ :type_of))
    nil))

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

; Generators

(defmacro gen-class-pred (nm)
  ; (classpred string) -> lambda
  ; Generates a class predicte in the form of
  ; (defun nm? (o)) -> t | nil
  (defq isname (sym (cat nm "?")))
  `(progn
    (defq ,(defq mclass (gensym)) (classkw ,nm))
    (defun ,isname (o)
      (class-type-of? o ,mclass))))

; Test samples
; (defclass a (v) nil
;   (def this :x 2 :y 7 z v)
;   (defmethod f (this))
;   (defmethod :f (this)))

; (defclass b () (a 3)
;   (def this :q 1 :r 3)
;   (defmethod bs (this) ))

; (defclass c () (b))
