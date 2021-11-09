;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; data_ast - Data Abstract Syntax Tree
;
; Library supporting conversions to and from
; data AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/class/reflect.inc")

(defq
  data-scalar-set       (xset-k :str :sym :num :keyword :boolean)
  data-sequence-set     (xset-k :list :array)
  data-collection-types (xmap-kv
    :xmap xmap
    :emap emap
    :xset xset))

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
      (values (get :children this))
      (cat (list (. this :type)) _)))

  (defmethod :to_object (this)
    ; (. ast :to_object) -> object
    ; Produces an object representation of
    ; the original data object that built
    ; the AST
    (reduce
      (#(push %0 (. %1 :to_object)))
      (values (get :children this)) (list)))

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
      (values (get :children this))))
  )

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
    (defq v (get :value this))
    (when (and (eql (get :clz this) :str) (find "_" v))
      (setq v (join (split v "_") " ")))
    v)

  (defmethod :show_tree (this &optional indent)
    (defq ind (pad "" indent))
    (print ind "-> " (. this :type) " " (get :clz this) " . "(get :value this)))
  )

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
      (values (get :children this))))

  (defmethod :tolist (this &rest _)
    (.super this :tolist (get :clz this)))

  (defmethod :to_object (this)
    (reduce
      (#(push %0 (. %1 :to_object)))
      (values (get :children this))
      (if (eql (get :clz this) :list)
        (list)
        (array))))

  ; collection-sequence type constructor
  (each (#(ast-for this %0)) _)
  )

; Collection structured data
; AST type that captures
; :emap
; :xmap
; :xset

(defclass collection-kv (_ clz colltype) (ast-collection)
  (def this
    :colltype colltype
    :clz      clz)

  (defmethod :show_tree (this &optional indent)
    (defq ind (pad "" indent))
    (print ind "-> " (. this :type) " : " (_mclass-id (get :colltype this)))
    (each
      (lambda (_c)
        (. _c :show_tree (+ indent 2)))
      (values (get :children this))))

  (defmethod :tolist (this &rest _)
    (.super this :tolist (get :clz this)))

  (defmethod :to_object (this)
    (defq inst ((get :colltype this)))
    ((into-fn inst) inst (.super this :to_object)))

  ; collection-kv type constructor
  (when (class-inst? _)
    (each (#(ast-for this %0)) (entries _)))
  )

; Class data types
(defclass ast-class (v clz module) (ast)
  (def this
    :clz_name   clz
    :path       module)

   (defmethod :tolist (this &rest _)
    (.super this :tolist (get :clz_name this) (get :path this)))

   (defmethod :to_object (this)
    (import (get :path this))
    (defq
      dmap  (entries (. (first (values (get :children this))) :to_object)))
    (reduce
      (lambda (acc (_k _v))
        (set acc _k _v) acc) dmap ((eval (get :clz_name this)))))

   (defmethod :show_tree (this &optional indent))

   ; Class type constructor from object
   ; ignored when rehydrating
   (when v
    (ast-for this
      (reduce
        (lambda (e (k v))
          (sets! e k v))
          (inst-data v) (xmap))))
   )

(defun ast-for (p v)
  ; (ast-for ast data) -> ast
  ; Recursivly walks the data (v) construct and builds
  ; the AST tree nodes
  (defq
    nt  (last (type-of v))
    clz (class-type v))
  (. p :add_node
    (cond
      ((or (func? v) (lambda? v) (macro? v))
        (from-scalar "func or lambda or macro" :str))
      ((gets data-scalar-set nt)
        (when (eql nt :str)
          (setq v (join (split v " ") "_")))
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
      ((gets data-sequence-set nt)
        (collection-sequence v nt))
      ((and
        (eql nt :hmap)
        (defq ct (gets data-collection-types clz)))
        (collection-kv v clz ct))
      (clz
        (ast-class
          v
          (sym (slice 1 -1 clz))
          (_minst-module (tolist (get :vtable v)))))
      ((eql nt :hmap)
        nil)
      (t
        (throw "No handling type yet" nt)))))

(defun data-to-ast (data &optional node)
  ; (data-to-ast data [astnode]) -> ast
  ; Converts a data object to an ast
  (cond
    ((nil? node)
      (ast-for (setd node (ast)) data))
    ((and
      node
      (class-inst? node)
      (find :ast (. node :type_of)))
      (ast-for node data))
    (t
      (throw "Not a data_ast type" data)))
  node)

(defun ast-from (p v)
  ; (ast-from ast data) -> ast
  ; Recursivly walks the ast list data (v) construct
  ; and builds the AST tree nodes
  (. p :add_node
    (cond
      ((gets data-scalar-set (first v))
        (ast-scalar (second v) (first v)))
      ((eql (first v) :collection-sequence)
        (defq clz (second v))
        (reduce
          (lambda (acc el) (ast-from acc el) acc)
          (slice 2 -1 v) (collection-sequence '() clz)))
      ((eql (first v) :collection-kv)
        (defq clz (second v))
        (reduce
          (lambda (acc el) (ast-from acc el) acc)
          (slice 2 -1 v)
          (collection-kv
            '()
            clz
            (gets data-collection-types clz))))
      ((eql (first v) :ast-class)
        (defq
          cl (ast-class nil (second v) (elem-get 2 v)))
        (ast-from cl (elem-get 3 v))
        cl)
      (t
        (throw "Not an ast list type" (first v))))))

(defun list-to-ast (astlist)
  ; (list-to-ast astlist) -> ast
  ; Converts a list generated by (. ast :tolist)
  ; to an AST
  (defq node (ast))
  (ast-from node (second astlist))
  node)

; (defclass a () nil)
; (defclass b () (a) (def this :x nil))
; (defq b1 (b))
; (set b1 :x 7)

; (defq root (ast))
; (data-to-ast b1 root)
; (. root :tolist)
; (tolist (first (. root :to_object)))

; (defq r1 (ast))
; (data-to-ast (list 1 2 3) r1)
; (defq r2 (list-to-ast (. r1 :tolist)))

; (defq r3 (ast))
; (data-to-ast (xmap-kv :a 1 :b 2 :c 3) r3)
; (defq r4 (list-to-ast (. r3 :tolist)))
; (. r4 :tolist)
;
