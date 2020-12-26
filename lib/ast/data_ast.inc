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
    ; (. ast :tolist) -> ast
    ; Exports the data AST as an
    ; encoded list tree
    (reduce
      (#(push %0 (. %1 :tolist)))
      (values (get :children_nodes this))
      (cat (list (. this :type)) _)))

  (defmethod :to_object (this)
    ; (. ast :to_object) -> object
    ; Produces an object representation of
    ; the original data object that built
    ; the AST
    (reduce
      (#(push %0 (. %1 :to_object)))
      (values (get :children_nodes this)) (list)))

  (defmethod :show_tree (this &optional indent)
    ; (. ast :show_tree) -> ast
    ; Prints the AST hierarchy with some
    ; additional meta data
    (setd indent 0)
    (defq ind (pad "" indent))
    (print ind "-> " (. this :type))
    (each
      (lambda (_c)
        (. _c :show_tree (+ indent 2)))
      (values (get :children_nodes this)))))

; Scalar data
; AST Type that captures
;   :str      "aaaa"
;   :num      0 | 1 | n
;   :sym      x.
;   :keyword  :x.
;   :boolean  t | nil
(defclass ast-scalar  (v clz) (ast)
  (def this
    :value  v
    :clz    clz)

  (defmethod :tolist (this)
    (list (get :clz this) (get :value this)))

  (defmethod :to_object (this)
    (get :value this))

  (defmethod :show_tree (this &optional indent)
    (defq ind (pad "" indent))
    (print ind "-> " (. this :type) " " (get :clz this) " . "(get :value this))))

; Base collection data type
(defclass ast-collection () (ast))

; Collection sequence data
; AST type that captures
; :list
; :array
(defclass collection-sequence (_ clz) (ast-collection)
  (def this
    :clz clz)

  (defmethod :show_tree (this &optional indent)
    (defq ind (pad "" indent))
    (print ind "-> " (. this :type) " : " (get :clz this))
    (each
      (lambda (_c)
        (. _c :show_tree (+ indent 2)))
      (values (get :children_nodes this))))

  (defmethod :to_object (this)
    (reduce
      (#(push %0 (. %1 :to_object)))
      (values (get :children_nodes this))
      (if (eql (get :clz this) :list)
        (list)
        (array))))

  ; collection-sequence type constructor
  (each (#(ast-for this %0)) _))

; Collection structured data
; AST type that captures
; :emap
; :xmap
; :xset

(defclass collection-kv (_ colltype) (ast-collection)
  (def this
    :colltype colltype)

  (defmethod :show_tree (this &optional indent)
    (defq ind (pad "" indent))
    (print ind "-> " (. this :type) " : " (_mclass-id (get :colltype this)))
    (each
      (lambda (_c)
        (. _c :show_tree (+ indent 2)))
      (values (get :children_nodes this))))

  (defmethod :to_object (this)
    (defq inst ((get :colltype this)))
    ((into-fn inst) inst (.super this :to_object)))

  ; collection-kv type constructor
  (each (#(ast-for this %0)) (entries _)))

; Class data types
(defclass ast-class (v clz) (ast)
  (def this
    :clz_name   (sym (slice 1 -1 clz))
    :path       (_minst-module (tolist (get :vtable v))))

   (defmethod :tolist (this)
    (.super this :tolist (get :clz_name this) (get :path this)))

   (defmethod :to_object (this)
    (import (get :path this))
    (defq
      dmap  (entries (. (first (values (get :children_nodes this))) :to_object)))
    (reduce
      (lambda (acc (_k _v))
        (set acc _k _v) acc) dmap ((eval (get :clz_name this)))))

   (defmethod :show_tree (this &optional indent))

   ; Class type constructor
  (ast-for this
    (reduce
      (lambda (e (k v))
        (sets! e k v))
        (inst-data v) (xmap))))

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
            (ast-scalar
              v
              (case nt
                (:sym
                  (cond
                    ((or (eql v nil) (eql v t))
                      :boolean)
                    ((eql (first v) ":")
                      :keyword)
                    (t
                      nt)))
                (t
                  nt))))
            ; (from-scalar v nt))
          ((gets data-sequence-set nt)
            (collection-sequence v nt))
          ((and (eql nt :hmap) (gets data-collection-set clz))
            (collection-kv
              v
              (case clz
                (:emap emap)
                (:xmap xmap)
                (:xset xset))))
            ; (from-collection v clz))
          (clz
            (ast-class v clz))
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

(defclass b () (a) (def this :x nil))

(defq b1 (b))
(set b1 :x 7)
(defq root (ast))
(to-ast b1 root)
