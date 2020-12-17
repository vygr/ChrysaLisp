;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reflect - Reflection Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
  +class_prefix+ "*class")

(defmacro meta-class? (_)
  ; (class-inst? symbol) -> t | nil
  ; Predicate for answering if a symbol is of type 'class'
  `(if (and (seq? (defq ,(defq __ (gensym)) ,_)) (> (length ,__) 2))
    (starts-with +class_prefix+ (last (elem 2 ,__)))
    nil))

(defmacro meta-class (_)
  `(progn
    (defq
      ,(defq __ (gensym)) ,_)
    (if (meta-class? ,__)
      (slice
        (inc (find "_" (str (last (elem 2 ,__)))))
        -2 (last (elem 2 ,__)))
      :none)))

(defmacro class-inst? (_)
  ; (class-inst? object) -> t | nil
  ; Predicate for answering if the object is of type class
  `(cond
    ((env? (defq ,(defq __ (gensym)) ,_))
      (catch (find :class (. ,__ :type_of)) nil))
    (t nil)))

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
    ((class-inst? ,_obj)
      (find ,_ttype (. ,_obj :type_of)))
    (t nil))))
