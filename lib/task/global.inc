;;;;;;;;;;;;;;;;;;;;
; global task object
;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defclass Global (fnc_create fnc_destroy fnc_poll) (xmap)
	; (Global fnc_create fnc_destroy fnc_poll) -> global
	(def this :size 0 :create fnc_create :destroy fnc_destroy :poll fnc_poll)

	(defmethod :refresh (this)
		; (. global :refresh) -> t | nil
		;scan known nodes and update node map
		(defq _nodes (mail-nodes) _old_keys (list) _mutated nil)
		(. this :each (lambda (key val) (push _old_keys key)))
		;test for new nodes
		(each (lambda (_node)
			(unless (find _node _old_keys)
				(setq _mutated t)
				;must (cat _node) to convert to pure string key !
				(. this :insert (cat _node) ((get :create this) _node)))) _nodes)
		;test for vanished nodes
		(each (lambda (_node)
			(unless (find _node _nodes)
				(setq _mutated t)
				((get :destroy this) (. this :find _node))
				(. this :erase _node))) _old_keys)
		(def this :size (length _nodes))
		;poll all nodes
		(. this :each (lambda (_k _v) ((get :poll this) _v)))
		_mutated)

	(defmethod :close (this)
		; (. global :close)
		;close tasks
		(. this :each (lambda (_k _v) ((get :destroy this) _v))))

	(defmethod :size (this)
		; (. global :size) -> size
		;size of tasks
		(get :size this))
	)
