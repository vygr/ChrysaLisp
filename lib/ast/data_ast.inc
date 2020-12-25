;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; data_ast - Data Abstract Syntax Tree
;
; Library supporting conversions to and from
; data AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (import "lib/class/reflect.inc")

(defq
  data_ast_module     *stream_name*
  data-scalar-set     (xset-k :str :sym :num)
  data-sequence-set   (xset-k :list :array)
  data-collection-set (xset-k :xmap :emap :xset))

; Base AST class
(defclass ast () (xnode)
  (defmethod :type (this)
    ; (. ast :type) -> keyword
    ; Get's last type identifying self
    (last (. this :type_of)))

  (defmethod :tolist (this &rest _)
    (reduce
      (#(push %0 (. %1 :tolist)))
      (values (get :children_nodes this))
      (cat (list (. this :type)) _))))

; Scalar data types
(defclass ast-scalar  (v) (ast)
  (def this :value v)
  (defmethod :tolist (this)
    (list (. this :type) (get :value this))))

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

; Base collection data type
(defclass ast-collection () (ast))

; Sequence data collections types (list, array)
(defclass collection-sequence (_) (ast-collection)
  (each (#(ast-for this %0)) _))
(defclass collection-array      (_) (collection-sequence _))
(defclass collection-list       (_) (collection-sequence _))

(defun from-sequence (v sqt)
  (cond
    ((eql sqt :list)
      (collection-list v))
    (t
      (collection-array v))))

; Associative data collection types
(defclass collection-kv (_) (ast-collection)
  (each (#(ast-for this %0)) (entries _)))

(defclass collection-emap (_) (collection-kv _)
  ; (each (#(print %0)) (entries _))
  )

(defclass collection-xmap       (_) (collection-kv _))
(defclass collection-xset       (_) (collection-kv _))

(defun from-collection (v col)
  (case col
    (:xmap (collection-xmap v))
    (:emap (collection-emap v))
    (:xset (collection-xset v))))

; Class data types
(defclass ast-class (v clz) (ast)
  (def this
    :clz_name   (sym (slice 1 -1 clz))
    :path       (_minst-module (tolist (get :vtable v))))
   (defmethod :tolist (this)
    (.super this :tolist (get :clz_name this) (get :path this)))
   ; Class type constructor
  (ast-for this
    (reduce
      (lambda (e (k v))
        (sets! e k v))
        (inst-data v) (emap))))

(defun from-class (v clz)
  (ast-class v clz))

; Recusive walk and produce data AST
(defun ast-for (p v)
  (. p :add_node
    (defq
      nt  (last (type-of v))
      clz (class-type v)
      nn
        (cond
          ((or (func? v) (lambda? v) (macro? v))
            (from-scalar "func or lambda or macro" :str))
          ((gets data-scalar-set nt)
            (from-scalar v nt))
          ((gets data-sequence-set nt)
            (from-sequence v nt))
          ((and (eql nt :hmap) (gets data-collection-set clz))
            (from-collection v clz))
          (clz
            (from-class v clz))
          ((eql nt :hmap)
            nil)
          (t
            (throw "No handling type yet" nt))))))

(defun to-ast (data &optional ast-type)
  (cond
    ((nil? ast-type)
      (ast-for (setd ast-type (ast)) data))
    ((and
      ast-type
      (class-inst? ast-type)
      (find :ast (. ast-type :type_of)))
      (ast-for ast-type data))
    (t
      (throw "Not a data_ast type" data)))
  ast-type)

(defclass a () nil)
(defclass b (x) (a) (def this :x x))
(defq b1 (b 7))
; (defq root (ast))
; (ast-for root b1)
