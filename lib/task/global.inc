;;;;;;;;;;;;;;;;;;;;
; global task object
;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defclass Global (fnc_create fnc_destroy fnc_poll) nil
	; (Global fnc_create fnc_destroy fnc_poll) -> global
	(def this :node_map (xmap) :size 0
		:create fnc_create :destroy fnc_destroy :poll fnc_poll)

	(defmethod :refresh (this)
		; (. global :refresh) -> t | nil
		;scan known nodes and update node map
		(defq _nodes (mail-nodes) _old_nodes (list) _node_map (get :node_map this) _mutated nil)
		(. _node_map :each (lambda (key val) (push _old_nodes key)))
		;test for new nodes
		(each (lambda (_node)
			(unless (find _node _old_nodes)
				(setq _mutated t)
				;must (cat _node) to convert to pure string key !
				(. _node_map :insert (cat _node) ((get :create this) _node)))) _nodes)
		;test for vanished nodes
		(each (lambda (_node)
			(unless (find _node _nodes)
				(setq _mutated t)
				((get :destroy this) (. _node_map :find _node))
				(. _node_map :erase _node))) _old_nodes)
		(def this :size (length _nodes))
		_mutated)

	(defmethod :poll (this)
		; (. global :poll)
		;poll tasks
		(. (get :node_map this) :each (lambda (_k _v) ((get :poll this) _v))))

	(defmethod :close (this)
		; (. global :close)
		;close tasks
		(. (get :node_map this) :each (lambda (_k _v) ((get :destroy this) _v))))

	(defmethod :find (this key)
		; (. global :find key) -> nil | val
		;find val for key
		(. (get :node_map this) :find key))

	(defmethod :size (this)
		; (. global :size) -> size
		;size of tasks
		(get :size this))
	)
