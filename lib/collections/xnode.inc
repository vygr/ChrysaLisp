;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xnode is a raw tree node that
; can contain unique children nodes
; Supports adding and removing children
; Default unique_id is generated using
; the hash function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass xnode ()
  ; Base tree node abstraction
  (def this
    :children_nodes (xmap)
    :parent         nil
    :xnode          t)

  (defmethod :get_parent (this)
    ; (. xnode :get_parent) -> xnode | nil
    ; Retrieves the parent node of this node
    (obj-ref (get :parent this))
    ; (get :parent this)
    )

  (defmethod :set_parent (this parent)
    ; (. xnode :set_parent parent) -> xnode
    ; Sets the parent node of this node
    (set this :parent (weak-ref parent))
    ; (set this :parent parent)
    this)

  (defmethod :root_node (this)
    ; (. xnode :root_node) -> xnode
    ; Walks up hierarchy to the root
    (defq cnode this)
    (while (defq pnode (. cnode :get_parent))
      (setq cnode pnode))
    cnode)

  (defmethod :children (this)
    ; (. xnode :children) -> list
    ; Return childrenlist of unique_id / value tuples
    (entries (get :children_nodes this)))

  (defmethod :siblings (this)
    ; (. xnode :siblings) -> list
    ; Returns siblings list of unique_id / value tuples
    ; from my parent if not root
    (defq uid (. this :unique_id))
    (cond
      ((. this :get_parent)
        (filter (#(not (eql (first %0) uid)))
          (entries (get :children_nodes (. this :get_parent)))))
      (t '())))

  (defmethod :unique_id (this)
    ; (. xnode :unique_id) -> unique_id
    ; Returns the has of this object
    ; Key override of subclasses to speacialize
    (hash this))

  (defmethod :add_node (this child)
    ; (. xnode :add_node child) -> xnode
    ; Adds a child node if unique_id not in children
    (defq hsh (. child :unique_id))
    (when (nil? (gets (get :children_nodes this) hsh))
      (. child :set_parent this)
      (sets! (get :children_nodes this) hsh child))
    child)

  (defmethod :drop_node (this child)
    ; (. xnode :drop_node child) -> xnode
    ; Removes a child node if unique id in children
    (defq hsh (. child :unique_id))
    (when (gets (get :children_nodes this) hsh)
      (. child :set_parent nil)
      (drop! (get :children_nodes this) hsh))
    child)

  (defmethod :move (this new_parent)
    ; (. xnode :move new_parent) -> xnode
    ; Moves this node to different new parent
    (defq p (get :parent this))
    (. new_parent :add_node (if p (. p :drop_node this) this)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; named-xnode is a specialization of
; xnode that uses names as the unique
; id and supports realizing the full
; tree path up to the node requested
; Overrides the xnode :unique_id method
; Supports constructs like
; .foo.bar.sizzle.steak --or--
; /foo/bar/sizzle/steak --etc--
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass named-xnode (name &optional parent)
  ; Named tree
  (def (defq this (xnode))
    :node_name    name
    :named_xnode  t)

  (defmethod :unique_id (this)
    ; (. named-xnode :unique_id) -> string
    ; Overrides xnode :unique_id method
    ; Returns this nodes name
    (get :node_name this))

  (defmethod :full_name (this &optional suffix)
    ; (. named-xnode :full_name [suffix]) -> string
    ; Returns the full name path string to this node
    ; By bottom up walk to root
    (setd suffix "")
    (defq
      nm (get :node_name this)
      pr (. this :get_parent))
    (cond
      (pr
        (. pr :full_name (str nm suffix)))
      (t
        (str nm suffix))))

  (when parent
    (. parent :add_node this))
)

(defun xnode? (this)
  ; (xnode? obj) -> t | nil
  (and (env? this) (get :xnode this)))

(defun named-xnode? (this)
  ; (named-xnode? obj) -> t | nil
  (and (env? this) (get :named_xnode this)))
