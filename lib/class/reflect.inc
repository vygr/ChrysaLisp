;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reflect - Reflection Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro class? (_)
  ; (class-inst? object) -> t | nil
  ; Predicate for answering if the object is of type class
  `nil)

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

(defun type-of? (obj ttype)
  ; (type-of? object ctype) -> t | nil
  ; Predicate for answering of object is
  ; of a certain type.
  ; (cond
  ;   ((find ttype (type-of obj)) t)
  ;   ((class-inst? obj)
  ;     (print "object is a class instance of " (. obj :type_of))
  ;     (print "Checking if is of type " ttype)
  ;     (find ttype (. obj :type_of)))
  ;   (t nil))
  )

