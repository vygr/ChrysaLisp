;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; data_ast - Data Abstract Syntax Tree
;
; Library supporting conversions to and from
; data AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/class/reflect.inc")

(defq
  data_ast_module     *stream_name*
  data-scalar-set     (xset-k :str :sym :num)
  data-sequence-set   (xset-k :list :array)
  data-collection-set (xset-k :xmap :emap :xset :xnode))

; Base AST class
(defclass ast () (xnode)
  ; (defmethod :module (this)
  ;   data_ast_module)

  (defmethod :type (this)
    (last (. this :type_of))))

; Scalar data types
(defclass ast-scalar  (v) (ast)
  (def this :value v)
  (defmethod :value (this)
    (get :value this)))

(defclass scalar-str  (v) (ast-scalar v))
(defclass scalar-sym  (v) (ast-scalar v))
(defclass scalar-kw   (v) (ast-scalar v))
(defclass scalar-num  (v) (ast-scalar v))
(defclass scalar-bool (v) (ast-scalar v))

(defun from-scalar (v st)
  (case st
    (:num (scalar-num v))
    (:str (scalar-str v))
    (:sym
      (cond
        ((or (eql v nil) (eql v t))
          (scalar-bool v))
        ((eql (first v) ":")
          (scalar-kw v))
        (t
          (scalar-sym v))))))

; Collection data types
(defclass ast-collection () (ast))
(defclass collection-sequence (_) (ast-collection)
  (each (#(ast-for this %0)) _))

(defclass collection-array      (_) (collection-sequence _))
(defclass collection-list       (_) (collection-sequence _))

(defclass collection-emap       () (ast-collection)
  )
(defclass collection-xmap       () (ast-collection)
  )
(defclass collection-mapentry   () (ast-collection)
  )
(defclass collection-key        () (ast-collection)
  )
(defclass collection-value      () (ast-collection)
  )
(defclass collection-xset       () (ast-collection)
  )
(defclass collection-xnode      () (ast-collection)
  )
; (defclass collection-rbtree     () (ast-collection))
(defun from-sequence (v sqt)
  (if (eql sqt :list)
    (collection-list v)
    (collection-array v)))

(defun from-collection (v col)
  (case col
    (:xmap (collection-xmap))
    (:emap (collection-emap))
    (:xset (collection-xset))
    (:xnode (collection-xnode))))

; Class data types
(defclass ast-class    (clz) (ast)
  (def this :clz clz))

(defun from-class (v clz)
  (ast-class clz))

; Recusive walk and produce data AST
(defun ast-for (p v)
  (. p :add_node
    (defq
      nt  (last (type-of v))
      clz (class-type v)
      nn
        (cond
          ((func? v)
            (throw "Function type not supported " v))
          ((or (lambda? v) (macro? v))
            (throw "Type not supported " (elem 0 v)))
          ((gets data-scalar-set nt)
            (print "Scalar match")
            (from-scalar v nt))
          ((gets data-sequence-set nt)
            (print "Sequence match")
            (from-sequence v nt))
          ((and (eql nt :hmap) (gets data-collection-set clz))
            (print "Collection match")
            (from-collection v clz))
          (clz
            (print "Class match")
            (from-class v clz))
          ((eql nt :hmap)
            (throw "Environment not supported" t))
          (t
            (throw "No handling type yet" nt))))))
