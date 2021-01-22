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
		(defq _keys (mail-nodes) _old_keys (list) _mutated nil)
		(. this :each (lambda (key val) (push _old_keys key)))
		;test for new nodes
		(each (lambda (_key)
			(unless (find _key _old_keys)
				(setq _mutated t)
				;must (cat _key) to convert to pure string key !
				(. this :insert (cat _key) ((get :create this) _key)))) _keys)
		;test for vanished nodes
		(each (lambda (_key)
			(unless (find _key _keys)
				(setq _mutated t)
				((get :destroy this) _key (. this :find _key))
				(. this :erase _key))) _old_keys)
		(def this :size (length _keys))
		;poll all nodes
		(. this :each (get :poll this))
		_mutated)

	(defmethod :close (this)
		; (. global :close)
		;close tasks
		(. this :each (get :destroy this)))

	(defmethod :size (this)
		; (. global :size) -> size
		;size of tasks
		(get :size this))
	)
