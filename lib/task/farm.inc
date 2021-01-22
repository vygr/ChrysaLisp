;;;;;;;;;;;;;;;;;;
; farm task object
;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defclass Farm (fnc_create fnc_destroy size) (xmap)
	; (Farm fnc_create fnc_destroy size) -> farm
	(def this :create fnc_create :destroy fnc_destroy)
	(defq _nodes (mail-nodes))
	(times size (fnc_create _nodes))

	(defmethod :refresh (this)
		; (. farm :refresh) -> t | nil
		;scan known nodes and update map
		(defq _nodes (mail-nodes) _old_keys (list) _mutated nil)
		(. this :each (lambda (key val) (push _old_keys key)))
		;test for vanished nodes
		(each (lambda (_key)
			(unless (find (slice (const long_size) -1 _key) _nodes)
				(setq _mutated t)
				((get :destroy this) _key (. this :find _key))
				(. this :erase _key)
				((get :create this) _nodes))) _old_keys)
		_mutated)

	(defmethod :close (this)
		; (. farm :close)
		;close tasks
		(. this :each (get :destroy this)))
	)
